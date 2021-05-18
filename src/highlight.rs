//! A robust syntax highlighting module built on syntect.
//! It should be able to run syntax highlighting tasks in
//! the background and cache results.

use std::{ops::Range, thread};

use crossbeam_channel::{unbounded, Receiver, Sender};
use druid::{ExtEventSink, Selector};
use syntect::{
    highlighting::{HighlightState, Highlighter, RangedHighlightIterator, Style},
    parsing::{ParseState, ScopeStack, SyntaxReference},
};
use xi_rope::{LinesMetric, Rope};

use crate::{
    editor::EDITOR_ID,
    settings::{self, THEME},
};

/// The number of lines in a cache chunk. Lowering the amount takes more RAM
/// but makes the highlighting faster after an edit.
const LINES_PER_STATE: usize = 1000;

/// Druid command triggered when new syntax highlighting has been parsed.
/// The payload is the first new highlighted line.
pub const NEW_HIGHLIGHTING: Selector<usize> = Selector::new("ne2.highlight.new_highlighting");

/// The minimum number of lines before triggering a druid notification.
pub const LINES_PER_COMMAND: usize = 50;

/// A command sent by the master thread.
enum MasterCommand {
    /// Invalidates the specified line and updates the buffer.
    Invalidate(usize, Rope),
}

/// A command sent by the worker thread.
enum WorkerCommand {
    /// A line parsed
    Line(usize, Vec<(Style, Range<usize>)>),
}

/// High-level syntax highlighting cache.
pub struct HighlightCache {
    /// The highlighted lines.
    highlights: Vec<Vec<(Style, Range<usize>)>>,
    /// The command sender to the worker thread.
    sender: Sender<MasterCommand>,
    /// The command receiver from the worker thread.
    receiver: Receiver<WorkerCommand>,
}

impl HighlightCache {
    /// Creates a new syntax highlighting cache.
    pub fn new(syntax: &'static SyntaxReference, buffer: Rope, sink: ExtEventSink) -> Self {
        let highlighter = Highlighter::new(&THEME);
        let highlights = Vec::with_capacity(buffer.measure::<LinesMetric>());

        let (s_master, r_master) = unbounded();
        let (s_worker, r_worker) = unbounded();

        thread::spawn(move || {
            let mut worker =
                HighlightCacheWorker::new(s_worker, r_master, syntax, highlighter, buffer, sink);
            worker.run();
        });

        Self {
            sender: s_master,
            receiver: r_worker,
            highlights,
        }
    }

    /// Clears the cache and invalidates the whole file.
    pub fn clear(&mut self, new_rope: Rope) {
        self.highlights.clear();
        // empty the receiver
        for _ in self.receiver.try_iter() {}
        self.invalidate(0, new_rope);
    }

    /// Polls the lines sent by the worker.
    pub fn poll(&mut self) {
        for command in self.receiver.try_iter() {
            match command {
                // pushing a new line
                WorkerCommand::Line(l, style) if l == self.highlights.len() => {
                    self.highlights.push(style);
                }

                // replacing a line in the cache
                WorkerCommand::Line(l, style) if l < self.highlights.len() => {
                    self.highlights[l] = style;
                }

                _ => panic!("One line has been skipped, this should never happen!"),
            }
        }
    }

    /// Loads from the cache the line highlight.
    pub fn highlight(&mut self, line_idx: usize) -> &[(Style, Range<usize>)] {
        self.poll();
        if let Some(style) = self.highlights.get(line_idx) {
            style
        } else {
            &[]
        }
    }

    /// Invalidates the cache when an edit is made to the text.
    /// This will delete the cache of the current line, and all the
    /// succeeding caches.
    pub fn invalidate(&mut self, line_idx: usize, new_rope: Rope) {
        self.sender
            .send(MasterCommand::Invalidate(line_idx, new_rope))
            .unwrap();
    }
}

/// A way to highlight text in a separate thread.
pub struct HighlightCacheWorker {
    /// The current line of the worker.
    current_line: usize,
    /// The number of lines since the last druid command.
    new_lines: usize,
    /// The initial state of the parser and syntax highlighter.
    initial_state: (ParseState, HighlightState),
    /// The cache of the different states. Every 1000 lines a new state is added.
    cache: Vec<(ParseState, HighlightState)>,
    /// The sender to the HighlightCache.
    sender: Sender<WorkerCommand>,
    /// The receiver from the HighlightCache.
    receiver: Receiver<MasterCommand>,
    /// The version of the buffer being highlighted.
    buffer: Rope,
    /// The highlighter.
    highlighter: Highlighter<'static>,
    /// The event sink to notify druid that new lines are available.
    sink: ExtEventSink,
}

impl HighlightCacheWorker {
    /// Creates a new worker.
    fn new(
        sender: Sender<WorkerCommand>,
        receiver: Receiver<MasterCommand>,
        syntax: &SyntaxReference,
        highlighter: Highlighter<'static>,
        buffer: Rope,
        sink: ExtEventSink,
    ) -> Self {
        let initial_state = (
            ParseState::new(syntax),
            HighlightState::new(&highlighter, ScopeStack::new()),
        );
        Self {
            current_line: 0,
            new_lines: 0,
            initial_state: initial_state.clone(),
            highlighter,
            buffer,
            sender,
            receiver,
            cache: vec![initial_state],
            sink,
        }
    }

    /// Runs the worker loop.
    pub fn run(&mut self) {
        while let Ok(command) = self.receiver.recv() {
            match command {
                MasterCommand::Invalidate(i, new_buffer) => {
                    // remove the invalidated caches
                    let invalidated_line = self.buffer.line_of_offset(i);
                    let mut current_cache = invalidated_line / LINES_PER_STATE;

                    // if the lines weren't highlighted in time, one of the cache may be missing
                    if current_cache >= self.cache.len() {
                        current_cache = self.cache.len() - 1;
                    }

                    self.cache.drain(current_cache..);

                    // reset the current line
                    let new_current_line = self.cache.len() * LINES_PER_STATE;
                    if new_current_line < self.current_line {
                        self.current_line = new_current_line;
                    }
                    self.new_lines = 0;

                    // clone the last cache
                    // we use pop() to avoid borrow issues
                    if let Some(last_cache) = self.cache.pop() {
                        self.cache.push(last_cache.clone());
                        self.cache.push(last_cache)
                    } else {
                        self.cache.push(self.initial_state.clone());
                    }

                    self.buffer = new_buffer;
                    self.highlight();
                }
            }
        }
    }

    /// Highlights the buffer until no line is left.
    pub fn highlight(&mut self) {
        let line_offset = self.buffer.offset_of_line(self.current_line);
        let mut lines = self.buffer.lines_raw(line_offset..);
        loop {
            // we still need to listen to the possible commands
            // This stops the current highlight job to handle the command.
            if !self.receiver.is_empty() {
                break;
            }

            // if we have no line left our job is done
            let line = match lines.next() {
                None => break,
                Some(line) => line,
            };

            let (parse_state, highlight_state) = self.cache.last_mut().unwrap();
            let ops = parse_state.parse_line(&line, &settings::PS);
            let iter =
                RangedHighlightIterator::new(highlight_state, &ops, &line, &self.highlighter);

            let style = iter.map(|(style, _, range)| (style, range)).collect();
            let command = WorkerCommand::Line(self.current_line, style);

            // notify HighlightCache
            if self.sender.send(command).is_err() {
                // if we can't send a message then this thread needs to be destroyed.
                // this means a new EditorState has been created, which is not tied to this thread.
                break;
            }

            self.new_lines += 1;
            self.current_line += 1;

            // notify Druid
            if self.new_lines == LINES_PER_COMMAND {
                let first_new_line = self.current_line - self.new_lines as usize;
                self.sink
                    .submit_command(NEW_HIGHLIGHTING, first_new_line, EDITOR_ID)
                    .unwrap();
                self.new_lines = 0;
            }

            // create a new state if needed
            if self.current_line + 1 == LINES_PER_STATE * self.cache.len() {
                self.cache.push(self.cache.last().unwrap().clone());
            }
        }

        if self.new_lines != 0 {
            let first_new_line = self.current_line - self.new_lines as usize;
            self.sink
                .submit_command(NEW_HIGHLIGHTING, first_new_line, EDITOR_ID)
                .unwrap();
            self.new_lines = 0;
        }
    }
}
