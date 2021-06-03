//! All the editor logic, from the text editing to the UI rendering.

use crate::{
    app::{AppState, SINK},
    file_explorer::OPEN_FILE_IN_EDITOR,
    highlight::{HighlightCache, LINES_PER_COMMAND, NEW_HIGHLIGHTING},
    settings::{self, FONT, THEME},
    utils::{LineEnding, ToColor},
    vim::{VimMotionKind, VimWord},
};
use druid::{
    piet::{PietText, Text, TextLayout as PietTextLayout, TextLayoutBuilder},
    text::{Attribute, RichText},
    Affine, ArcStr, Color, Data, Env, Event, ExtEventSink, FileInfo, FontWeight, Lens, LifeCycle,
    Point, Rect, RenderContext, Selector, Size, Target, TextAlignment, TextLayout, Widget,
    WidgetExt, WidgetId,
};
use unicode_segmentation::UnicodeSegmentation;

use std::{
    borrow::Cow,
    cell::Cell,
    fs, iter,
    ops::Range,
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
};
use syntect::{highlighting::FontStyle, parsing::SyntaxReference};
use xi_rope::{
    delta,
    engine::{Engine, RevId},
    tree, Interval, LinesMetric, Rope, RopeDelta, RopeInfo,
};

/// The command sent when a directory is open.
pub const OPEN_DIRECTORY: Selector<FileInfo> = Selector::new("ne2.editor.open_directory");

/// Command to exit just after saving the file.
pub const SAVE_FILE_AS_AND_EXIT: Selector<FileInfo> = Selector::new("ne2.editor.save_as_and_exit");

/// Command sent when the document changes and the app should notify the LSP server.
pub const DID_CHANGE: Selector<DeltaState> = Selector::new("ne2.editor.did_change");

/// Command sent when the document is saved and the app should notify the LSP server.
pub const DID_SAVE: Selector<()> = Selector::new("ne1.editor.did_save");

/// The id of the editor.
pub const EDITOR_ID: WidgetId = WidgetId::reserved(1);

/// The editor state.
#[derive(Clone, Data, Lens)]
pub struct EditorState {
    /// The cursor.
    pub cursor: Cursor,
    /// The engine containing the current file and all the editing history.
    /// Since Engine has no Clone semantics, we must wrap it in a Mutex.
    // As a result, changes in the engine cannot be correctly tracked by druid.
    // This is why we #[data(ignore)] this. Buffer modifications are tracked with
    // the delta field.
    #[data(ignore)]
    pub engine: Arc<Mutex<Engine>>,
    /// The first line displayed in the view. The float value makes it possible
    /// to scroll the view with a slow scroll wheel.
    line_offset: f64,
    /// The number of lines displayed in the text view.
    /// This is wrapped in a cell because the only way to mutate this is in lifecycle(&data).
    #[data(same_fn = "PartialEq::eq")]
    pub lines_displayed: Cell<usize>,
    /// The last edit state of the buffer.
    pub delta: DeltaState,
    /// The line ending style of the current file.
    pub line_ending: LineEnding,
    /// The indentation style of the current file.
    pub indent_style: IndentStyle,
    /// The programming language of the current file.
    pub language: String,
    /// The path of the file being edited. None if the file hasn't been saved to the disk yet.
    pub file: Option<Arc<Path>>,
}

/// An indenting style.
#[derive(Clone, Data, PartialEq)]
pub enum IndentStyle {
    /// Using tabs to indent code.
    Tabs(u8),
    /// Using spaces to indent code.
    Spaces(u8),
}

impl IndentStyle {
    /// Gets the indentation corresponding to this style.
    pub fn get(&self) -> String {
        match self {
            IndentStyle::Spaces(spaces) => " ".repeat(*spaces as usize),
            IndentStyle::Tabs(tabs) => "\t".repeat(*tabs as usize),
        }
    }
}

impl ToString for IndentStyle {
    fn to_string(&self) -> String {
        match self {
            IndentStyle::Spaces(1) => "1 space".to_owned(),
            IndentStyle::Spaces(spaces) => format!("{} spaces", spaces),
            IndentStyle::Tabs(1) => "1 tab".to_owned(),
            IndentStyle::Tabs(tabs) => format!("{} tabs", tabs),
        }
    }
}

/// An iterator over the values of the graphemes of a Rope.
pub struct GraphemeValuesIterator<'a> {
    /// The grapheme offset iterator.
    it: GraphemeIterator<'a>,
    /// The offset of the last grapheme.
    last_offset: usize,
}

impl<'a> GraphemeValuesIterator<'a> {
    /// Creates a new iterator over the values of the graphemes in a Rope.
    pub fn new(rope: &'a Rope, position: usize) -> Self {
        Self {
            it: GraphemeIterator::new(rope, position),
            last_offset: position,
        }
    }
}

impl<'a> Iterator for GraphemeValuesIterator<'a> {
    type Item = Cow<'a, str>;

    fn next(&mut self) -> Option<Self::Item> {
        self.it.next().map(|offset| {
            let next = self.it.rope().slice_to_cow(self.last_offset..offset);
            self.last_offset = offset;
            next
        })
    }
}

/// An iterator over the offsets of the graphemes of a Rope.
pub struct GraphemeIterator<'a>(tree::Cursor<'a, RopeInfo>);

impl<'a> GraphemeIterator<'a> {
    /// Creates a new iterator over the offsets of the graphemes in a Rope.
    pub fn new(rope: &'a Rope, position: usize) -> Self {
        Self(tree::Cursor::new(rope, position))
    }

    /// Returns a reference to the underlying rope.
    pub fn rope(&self) -> &'a Rope {
        self.0.root()
    }
}

impl<'a> Iterator for GraphemeIterator<'a> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        // there seem to be a bug when next_grapheme() is called multiple times
        // with the same Cursor. As a result, we must change cursors after every call.
        let next = self.0.next_grapheme();

        if let Some(offset) = next {
            self.0 = tree::Cursor::new(self.0.root(), offset);
        }

        next
    }
}

impl EditorState {
    /// Builds the editor's default state.
    pub fn new(start_file: Option<PathBuf>) -> Self {
        // we store first the buffer in a string before passing it to the rope.
        let string = if let Some(path) = &start_file {
            fs::read_to_string(path).expect("Invalid file path!")
        } else {
            String::new()
        };

        let file = start_file.as_deref().map(Arc::from);
        let rope = Rope::from(string);
        let first_line = rope.lines_raw(..).next();

        // get the syntax from the file extension or the first line of the file
        let language = EditorState::find_language(start_file, first_line.as_ref());

        let line_ending = first_line
            .and_then(|line| {
                if line.ends_with("\r\n") {
                    Some(LineEnding::Crlf)
                } else if line.ends_with('\n') {
                    Some(LineEnding::Lf)
                } else {
                    None
                }
            })
            // no line ending, infer from the OS.
            .unwrap_or_else(|| {
                if cfg!(windows) {
                    LineEnding::Crlf
                } else {
                    LineEnding::Lf
                }
            });

        // get the indentation style used in the file
        let mut indent = 0;
        let mut is_tabs = false;

        for (i, line) in rope.lines_raw(..).enumerate() {
            // don't try too hard to get the indentation style
            if i == 300 {
                break;
            }

            let mut it = line.chars();
            match it.next() {
                Some('\t') => is_tabs = true,
                Some(' ') => is_tabs = false,
                _ => continue,
            }

            indent += 1;
            for c in it {
                if !(" \t").contains(c) || c == '\t' && !is_tabs || c == ' ' && is_tabs {
                    break;
                }

                indent += 1;

                // this is probably just empty space in a file
                if indent > 8 {
                    indent = 0;
                    break;
                }
            }

            break;
        }

        let indent_style = if indent == 0 {
            IndentStyle::Spaces(4)
        } else if is_tabs {
            IndentStyle::Tabs(indent)
        } else {
            IndentStyle::Spaces(indent)
        };

        let engine = Engine::new(rope);

        let mut editor = Self {
            cursor: Cursor::default(),
            line_offset: 0.,
            lines_displayed: Cell::new(0),
            delta: DeltaState::new_empty(engine.get_head_rev_id()),
            engine: Arc::new(Mutex::new(engine)),
            file,
            indent_style,
            line_ending,
            language,
        };

        editor.update_cursor_over_size();
        editor.update_cursor_offset();
        editor
    }

    /// Finds the file's language when given a path and a first line.
    pub fn find_language(path: Option<PathBuf>, first_line: Option<&Cow<str>>) -> String {
        path.and_then(|f| f.extension().map(ToOwned::to_owned))
            .and_then(|ext| settings::PS.find_syntax_by_extension(ext.to_str().unwrap_or_default()))
            .or_else(|| {
                first_line
                    .clone()
                    .and_then(|l| settings::PS.find_syntax_by_first_line(&l))
            })
            .unwrap_or_else(|| settings::PS.find_syntax_plain_text())
            .name
            .clone()
    }

    /// Gets the latest revision of the buffer.
    #[inline]
    pub fn buffer(&self) -> Rope {
        self.engine.lock().unwrap().get_head().clone()
    }

    /// Gets the index of the first line on the screen.
    #[inline]
    pub fn first_line(&self) -> usize {
        self.line_offset as usize
    }

    /// Gets the index of the last line on the screen.
    #[inline]
    pub fn last_line(&self) -> usize {
        self.first_line() + self.lines_displayed.get()
    }

    /// Updates the language.
    pub fn update_language(&mut self) {
        let buffer = self.buffer();
        let first_line = buffer.lines_raw(..).next();
        self.language = EditorState::find_language(
            self.file.as_ref().map(|a| a.to_path_buf()),
            first_line.as_ref(),
        );
    }

    /// Updates the cursor over parameter.
    pub fn update_cursor_over_size(&mut self) {
        if self.cursor.mode == CursorMode::Boundary || self.cursor.x.is_none() {
            return;
        }

        let cursor_offset = self.cursor_offset();
        let buffer = self.buffer();
        self.cursor.over_size =
            Some(buffer.next_grapheme_offset(cursor_offset).unwrap() - cursor_offset);
    }

    /// Updates the editor's line offset if needed.
    pub fn update_line_offset(&mut self) {
        if self.first_line() > self.cursor.y {
            self.line_offset = self.cursor.y as f64;

        // the "+2" ensures that we still can see well the last line.
        } else if self.cursor.y + 2 > self.last_line() {
            let diff = self.cursor.y + 2 - self.last_line();
            self.line_offset = (self.first_line() + diff) as f64;
        }
    }

    /// Updates the number of lines displayed in the text view.
    #[inline]
    pub fn update_lines_displayed(&self, height: f64, line_height: f64) {
        self.lines_displayed
            .set((height / line_height) as usize + 1)
    }

    /// Removes the specified range from the buffer while registering the edit in the engine.
    pub fn remove(&mut self, range: Range<usize>) {
        let mut builder = delta::Builder::new(self.buffer().len());
        builder.delete(range);

        self.apply_change(builder, "");
    }

    /// Edits the specified range of the buffer while registering the edit in the engine.
    pub fn edit(&mut self, range: Range<usize>, text: &str) {
        let mut builder = delta::Builder::new(self.buffer().len());
        builder.replace(range, text.into());

        self.apply_change(builder, text);
    }

    /// Applies the change (an edit or a remove) and notifies the LSP server.
    fn apply_change(&mut self, builder: delta::Builder<RopeInfo>, text: &str) {
        self.invalidate_cursor();
        let mut engine = self.engine.lock().unwrap();
        let rev = engine.get_head_rev_id().token();
        let delta = builder.build();

        engine.edit_rev(0, 0, rev, delta.clone());
        self.delta
            .next(delta, engine.get_head_rev_id(), Arc::from(text));

        SINK.get()
            .unwrap()
            .submit_command(DID_CHANGE, self.delta.clone(), Target::Auto)
            .unwrap();
    }

    /// Sets the x coordinate of the cursor and updates Cursor::wanted_x if desired.
    pub fn set_cursor_x(&mut self, grapheme_idx: usize, update_wanted_x: bool) {
        self.invalidate_cursor();
        self.cursor.x = Some(grapheme_idx);

        if update_wanted_x {
            self.cursor.wanted_x = grapheme_idx;
        }

        if grapheme_idx == 0 {
            let buffer = self.buffer();
            let line = buffer.lines(self.cursor_y_offset()..).next();

            if line.unwrap_or_default().is_empty() {
                self.cursor.x = None;
            }
        } else {
            self.cursor.x = Some(grapheme_idx);
        }
    }

    /// Sets the y coordinate of the cursor.
    pub fn set_cursor_y(&mut self, line_idx: usize) {
        self.invalidate_cursor();
        self.cursor.y = line_idx;
    }

    /// Updates the cursor offset if needed.
    pub fn update_cursor_offset(&mut self) {
        if self.cursor.x_offset.is_some() && self.cursor.y_offset.is_some() {
            return;
        }

        let buffer = self.buffer();
        let line_offset = buffer.offset_of_line(self.cursor.y);
        self.cursor.y_offset = Some(line_offset);

        match self.cursor.x {
            Some(0) | None => self.cursor.x_offset = Some(0),
            Some(x) => {
                self.cursor.x_offset = Some(
                    GraphemeIterator::new(&buffer, line_offset)
                        .nth(x - 1)
                        .unwrap()
                        - line_offset,
                );
            }
        };
    }

    /// Lazily gets the byte offset of the x coordinate of the cursor.
    pub fn cursor_x_offset(&mut self) -> usize {
        self.update_cursor_offset();
        self.cursor.x_offset.unwrap()
    }

    /// Lazily gets the byte offset of the cursor's line.
    pub fn cursor_y_offset(&mut self) -> usize {
        self.update_cursor_offset();
        self.cursor.y_offset.unwrap()
    }

    /// Lazily gets the offset behind the cursor.
    pub fn cursor_offset(&mut self) -> usize {
        self.update_cursor_offset();
        self.cursor.x_offset.unwrap() + self.cursor.y_offset.unwrap()
    }

    /// Lazily gets the cursor over size.
    pub fn cursor_over_size(&mut self) -> usize {
        self.update_cursor_over_size();
        self.cursor.over_size.unwrap()
    }

    /// Invalidates the cached cursor offset and over size.
    pub fn invalidate_cursor(&mut self) {
        self.cursor.x_offset = None;
        self.cursor.y_offset = None;
        self.cursor.over_size = None;
    }

    /// Infers the text range affected by vim primitives
    pub fn infer_range(&mut self, motion: VimMotionKind, multiplier: usize) -> Range<usize> {
        let start;
        let end;
        match motion {
            VimMotionKind::Line => {
                start = self.cursor_y_offset();
                let buffer = self.buffer();
                let buffer_len = buffer.measure::<LinesMetric>();

                if self.cursor.y + multiplier > buffer_len {
                    end = buffer.offset_of_line(buffer_len + 1);
                } else {
                    end = buffer.offset_of_line(self.cursor.y + multiplier);
                }
            }

            VimMotionKind::FullWordStartForwards | VimMotionKind::WordStartForwards => {
                start = self.cursor_offset();

                let mut words = 0;

                // check for a word or a WORD depending on the motion
                let make_word = || {
                    if motion == VimMotionKind::FullWordStartForwards {
                        VimWord::full()
                    } else {
                        VimWord::regular()
                    }
                };

                let mut current_word = make_word();
                let mut count = 0;

                let buffer = self.buffer();

                for s in GraphemeValuesIterator::new(&buffer, start) {
                    if !current_word.check_grapheme(&s) {
                        words += 1;
                        current_word = make_word();
                    }

                    if words == multiplier {
                        break;
                    }
                    count += s.len();
                }
                end = start + count;
            }

            _ => unimplemented!(),
        }
        start..end
    }

    /// Deletes characters before the cursor. This is the action triggered by backspace.
    pub fn delete_backwards(&mut self) {
        let cursor_x = self.cursor.x.unwrap_or_default();

        // we can always delete a character unless we're on the first character of the first line
        if self.cursor.y == 0 && cursor_x == 0 {
            return;
        }

        // delete the newline and go the previous line
        if cursor_x == 0 {
            self.set_cursor_y(self.cursor.y - 1);
            self.cursor_to_eol();
            self.move_cursor(Direction::Left);
            self.delete_line_ending(self.cursor.y);

        // delete one character on the current line
        } else {
            let cursor_offset = self.cursor_offset();
            self.set_cursor_x(cursor_x - 1, true);

            let prev_grapheme = self.buffer().prev_grapheme_offset(cursor_offset).unwrap();
            self.remove(prev_grapheme..cursor_offset);
        }
    }

    /// Deletes the line ending of the specified line.
    pub fn delete_line_ending(&mut self, line_idx: usize) {
        let buffer = self.buffer();
        let line_offset = buffer.offset_of_line(line_idx);
        let line = buffer.lines_raw(line_offset..).next().unwrap();

        let line_ending_len = if line.ends_with("\r\n") {
            2
        } else if line.ends_with('\n') {
            1
        } else {
            0
        };

        let byte_offset = line_offset + line.len() - line_ending_len;
        self.remove(byte_offset..byte_offset + line_ending_len);
    }

    /// Gets the last grapheme for the line.
    pub fn last_grapheme_for_line(&self, line_idx: usize) -> Option<usize> {
        let buffer = self.buffer();
        let line_offset = buffer.offset_of_line(line_idx);
        buffer.lines(line_offset..).next().and_then(|line| {
            if line.is_empty() {
                None
            } else {
                Some(line.graphemes(true).count())
            }
        })
    }

    /// Changes the cursor to over mode.
    #[inline]
    pub fn cursor_to_over(&mut self) {
        self.cursor.mode = CursorMode::Over;
        self.update_cursor_over_size();
        self.update_cursor_offset();
    }

    /// Changes the cursor to boundary mode.
    #[inline]
    pub fn cursor_to_boundary(&mut self) {
        self.cursor.mode = CursorMode::Boundary;
        self.update_cursor_offset();
    }

    /// Moves the cursor to the end of the line. This means on the last character
    /// if the cursor mode is set on Over, or after the last character if the cursor
    /// mode is set on Boundary.
    pub fn cursor_to_eol(&mut self) {
        let x = self
            .last_grapheme_for_line(self.cursor.y)
            .unwrap_or_default();

        if self.cursor.mode == CursorMode::Boundary {
            self.set_cursor_x(x + 1, true);
        } else {
            self.set_cursor_x(x, true);
        }
    }

    /// Moves the cursor to the start of the line.
    #[inline]
    pub fn cursor_to_sol(&mut self) {
        self.set_cursor_x(0, true);
    }

    /// Moves the cursor to the specified line.
    #[inline]
    pub fn cursor_to_line(&mut self, line_idx: usize) {
        self.set_cursor_y(line_idx);
        self.cursor_to_sol();
    }

    /// Inserts a newline at the cursor position and matches the indentation level.
    pub fn cursor_to_newline(&mut self) {
        let cursor_offset = self.cursor_offset();
        let indent = self.insert_newline(cursor_offset);
        self.set_cursor_y(self.cursor.y + 1);
        self.set_cursor_x(indent, true);
    }

    /// Moves the cursor to the specified byte offset.
    pub fn cursor_to_byte(&mut self, byte_offset: usize) {
        let buffer = self.buffer();

        if buffer.is_empty() {
            return;
        }

        self.set_cursor_y(buffer.line_of_offset(byte_offset));
        let x = GraphemeIterator::new(&buffer, self.cursor_y_offset())
            .take_while(|&offset| offset <= byte_offset)
            .count();
        self.set_cursor_x(x, true);
    }

    /// Moves the cursor in the specified direction.
    pub fn move_cursor(&mut self, direction: Direction) {
        let boundary = if self.cursor.mode == CursorMode::Boundary {
            0
        } else {
            1
        };

        match direction {
            Direction::Up if self.cursor.y > 0 => {
                self.set_cursor_y(self.cursor.y - 1);

                let x = self
                    .last_grapheme_for_line(self.cursor.y)
                    .map(|last_char| self.cursor.wanted_x.min(last_char - boundary));
                self.set_cursor_x(x.unwrap_or_default(), false);
            }
            Direction::Down if self.cursor.y < self.buffer().measure::<LinesMetric>() => {
                self.set_cursor_y(self.cursor.y + 1);

                let x = self
                    .last_grapheme_for_line(self.cursor.y)
                    .map(|last_char| self.cursor.wanted_x.min(last_char - boundary));
                self.set_cursor_x(x.unwrap_or_default(), false)
            }
            Direction::Left if self.cursor.x > Some(0) => {
                let new_x = self.cursor.x.unwrap() - 1;
                self.set_cursor_x(new_x, true);
            }
            Direction::Right => {
                if let Some(x) = self.cursor.x {
                    let last_char = self.last_grapheme_for_line(self.cursor.y);

                    // allow positionning the cursor at the end of the line in insert mode
                    if Some(x + boundary) < last_char {
                        self.set_cursor_x(x + 1, true);
                    }
                }
            }
            _ => (),
        }
    }

    /// Inserts text at the cursor position.
    pub fn insert(&mut self, text: &str) {
        let offset = self.cursor_offset();
        self.edit(offset..offset, text);
        let x = self.cursor.x.unwrap_or_default() + text.graphemes(true).count();
        self.set_cursor_x(x, true);
    }

    /// Inserts a newline at the specified byte offset and matches the identation level
    /// of the current line.
    /// This respects the line ending of the current file.
    pub fn insert_newline(&mut self, byte_offset: usize) -> usize {
        let buffer = self.buffer();
        let indent = self.whitespace(buffer.offset_of_line(self.cursor.y));
        self.edit(
            byte_offset..byte_offset,
            &format!("{}{}", self.line_ending.to_chars(), indent),
        );

        indent.len()
    }

    /// Removes whitespace characters (space or tab) at the cursor position.
    pub fn remove_whitespace(&mut self) {
        let cursor_offset = self.cursor_offset();
        let buffer = self.buffer();
        let iter = GraphemeValuesIterator::new(&buffer, cursor_offset);
        let ws_count = iter.take_while(|s| " \t".contains(s.as_ref())).count();

        self.remove(cursor_offset..cursor_offset + ws_count);
    }

    /// Gets the whitespace characters (space or tab) at the specified position.
    pub fn whitespace(&self, byte_idx: usize) -> String {
        GraphemeValuesIterator::new(&self.buffer(), byte_idx)
            .take_while(|s| " \t".contains(s.as_ref()))
            .collect()
    }

    /// Gets the whitespace characters count (space or tab) at the specified position.
    pub fn whitespace_length(&self, byte_idx: usize) -> usize {
        GraphemeValuesIterator::new(&self.buffer(), byte_idx)
            .take_while(|s| " \t".contains(s.as_ref()))
            .count()
    }

    /// Gets the blank characters count (space tab, carriage return, line feed) at the specified position.
    pub fn blanks_length(&self, byte_idx: usize) -> usize {
        let mut count = 0;
        for grapheme in GraphemeValuesIterator::new(&self.buffer(), byte_idx) {
            count += match grapheme.as_ref() {
                // a CRLF line return is actually a single grapheme
                "\r\n" => 2,
                "\n" | "\t" | " " => 1,
                _ => break,
            }
        }
        count
    }

    /// Saves the buffer to the current file.
    pub fn save_to_file(&mut self) {
        let buffer = self.buffer();
        fs::write(
            self.file.as_ref().unwrap().as_os_str(),
            buffer.slice_to_cow(..).as_bytes(),
        )
        .expect("Could not write file to disk!");

        SINK.get()
            .unwrap()
            .submit_command(DID_SAVE, (), Target::Auto)
            .unwrap();
    }
}

/// An unique edit state of a Rope, identified by its RevId.
/// This is useful to easely keep track of modifications with druid.
#[derive(Clone)]
pub struct DeltaState {
    /// The delta.
    delta: Option<RopeDelta>,
    /// The revision after this delta.
    pub rev: RevId,
    /// The revision before this delta.
    pub old_rev: RevId,
    /// The changed text.
    pub text: ArcStr,
}

impl DeltaState {
    /// Creates a new delta state based on this delta.
    pub fn next(&mut self, delta: RopeDelta, rev: RevId, text: ArcStr) {
        *self = Self {
            delta: Some(delta),
            rev,
            old_rev: self.rev,
            text,
        };
    }

    /// Creates a new empty delta state.
    pub fn new_empty(rev: RevId) -> Self {
        Self {
            delta: None,
            rev,
            old_rev: rev,
            text: Arc::from(""),
        }
    }

    /// Gets the buffer before the change.
    #[inline]
    pub fn old_buffer(&self, engine: &Arc<Mutex<Engine>>) -> Rope {
        engine
            .lock()
            .unwrap()
            .get_rev(self.old_rev.token())
            .unwrap()
    }

    /// Returns the first modified char in this delta.
    /// This is useful for invalidating the succeding lines of the buffer.
    #[inline]
    pub fn first_modified_char(&self) -> usize {
        self.delta
            .as_ref()
            .map(|delta| delta.summary().0.start)
            .unwrap_or_default()
    }

    /// Returns the summary, assuming that the delta isn't None.
    pub fn summary(&self) -> (Interval, usize) {
        self.delta.as_ref().unwrap().summary()
    }
}

impl Data for DeltaState {
    fn same(&self, other: &Self) -> bool {
        self.rev == other.rev && self.delta.is_some() == other.delta.is_some()
    }
}

/// A cursor.
#[derive(Clone, Data, Debug, Default)]
pub struct Cursor {
    /// The wanted x coordinate in utf-8 graphemes.
    pub wanted_x: usize,
    /// The real x coordinate in utf-8 graphemes, if the line is too short to use the wanted x coordinate.
    pub x: Option<usize>,
    /// The y coordinate in lines.
    pub y: usize,
    /// The mode of the cursor.
    pub mode: CursorMode,
    /// The cached x offset since the start of the line, in bytes.
    pub x_offset: Option<usize>,
    /// The cached y offset in bytes.
    pub y_offset: Option<usize>,
    /// The over size in over mode.
    pub over_size: Option<usize>,
}

/// The mode of a cursor. Specifically, how a cursor interacts with the underlaying characters.
#[derive(Clone, Data, Debug, PartialEq)]
pub enum CursorMode {
    /// A cursor right over a character. This is how a cursor is represented in vim's normal mode.
    /// What we visually identify as a character is not always of the same size.
    /// This is why we have Cursor::over_size, tracking the grapheme size.
    /// ```text
    /// Cursor pos     Over size
    ///      --------------  
    ///     |       é      |
    ///      --------------
    /// ```
    Over,
    /// A cursor at a character boundary (right before or right after a character). This is how a cursor is
    /// represented in vim's insert mode.
    /// ```text
    ///                Cursor pos
    ///             é      |
    /// ```
    Boundary,
}

impl Default for CursorMode {
    fn default() -> Self {
        Self::Over
    }
}

/// Represents a cursor move
pub enum Direction {
    /// Move left
    Left,
    /// Move right
    Right,
    /// Move down
    Down,
    /// Move up
    Up,
}

/// The editor widget.
pub struct Ne2Editor {
    /// The underlaying druid text.
    text_view: TextLayout<RichText>,
    /// The line height.
    line_height: f64,
    /// The text layout used to represent the line numbers.
    line_numbers: TextLayout<String>,
    /// The width of the line numbers column.
    line_numbers_width: f64,
    /// The size of the normal-mode cursor when it's not on a character.
    normal_cursor_rect: Rect,
    /// The current syntax of the file.
    syntax: &'static SyntaxReference,
    /// The syntax highlighting cache.  
    /// Every 1000 lines a new state is created.
    /// Upon modification, all newer states are deleted.
    highlight_cache: Option<HighlightCache>,
}

impl Ne2Editor {
    /// Creates a new editor widget.
    pub fn new() -> Self {
        let line_numbers_color = THEME
            .settings
            .gutter_foreground
            .unwrap_or_else(|| THEME.settings.foreground.unwrap())
            .to_color();
        let mut line_numbers = TextLayout::new();
        line_numbers.set_text_color(line_numbers_color);
        line_numbers.set_text_alignment(TextAlignment::End);

        let font_color = THEME.settings.foreground.unwrap().to_color();
        let mut text_view = TextLayout::new();
        text_view.set_text_color(font_color);

        let syntax = settings::PS.find_syntax_plain_text();

        Self {
            normal_cursor_rect: Rect::default(),
            line_height: 0.,
            line_numbers_width: 0.,
            highlight_cache: None,
            line_numbers,
            text_view,
            syntax,
        }
    }

    /// Sets the size that the normal-mode cursor should occupy on an empty line.
    /// For monospaced fonts, it's pretty obvious, but it's not for regular fonts.
    pub fn update_normal_cursor(&mut self, text: &mut PietText) {
        // we will use the underscore character to get ta maximum width on a non-monospaced font.
        self.normal_cursor_rect = text
            .new_text_layout("_")
            .font(FONT.family.clone(), FONT.size)
            .build()
            .unwrap()
            .size()
            .to_rect();
    }

    /// Updates the text slice in the textview and the line numbers.
    pub fn update_textview(&mut self, data: &AppState, factory: &mut PietText, env: &Env) {
        // change the slice of text displayed
        let buffer = data.editor.buffer();
        let first_line = data.editor.first_line();
        let last_line = data.editor.last_line();
        let start = buffer.offset_of_line(first_line);
        let stop = if last_line > buffer.measure::<LinesMetric>() {
            buffer.len()
        } else {
            buffer.offset_of_line(last_line)
        };

        let txt = buffer.slice(start..stop);
        let mut rich_text = RichText::new(txt.slice_to_cow(..).into());
        let mut cursor = xi_rope::Cursor::new(&txt, 0);

        if let Some(highlight_cache) = &mut self.highlight_cache {
            for (i, offset) in iter::once(0)
                .chain(cursor.iter::<LinesMetric>())
                .take_while(|offset| start + offset < stop)
                .enumerate()
            {
                let ranges = highlight_cache.highlight(first_line + i);

                // the last line is going to be empty
                for (style, text_range) in ranges {
                    let text_range = text_range.start + offset..text_range.end + offset;

                    if style.font_style.contains(FontStyle::BOLD) {
                        rich_text
                            .add_attribute(text_range.clone(), Attribute::Weight(FontWeight::BOLD));
                    }

                    if style.font_style.contains(FontStyle::UNDERLINE) {
                        rich_text.add_attribute(text_range.clone(), Attribute::Underline(true));
                    }

                    if style.font_style.contains(FontStyle::ITALIC) {
                        rich_text.add_attribute(
                            text_range.clone(),
                            Attribute::Style(druid::FontStyle::Italic),
                        );
                    }

                    rich_text.add_attribute(
                        text_range.clone(),
                        Attribute::TextColor(
                            Color::rgb8(style.foreground.r, style.foreground.g, style.foreground.b)
                                .into(),
                        ),
                    );
                }
            }
        }
        self.text_view.set_text(rich_text);

        // change the line numbers text
        let mut line_numbers = String::with_capacity((stop - start) * 2);
        for i in first_line + 1..=last_line {
            line_numbers.push_str(&i.to_string());
            line_numbers.push('\n');
        }
        self.line_numbers.set_text(line_numbers);

        self.text_view.rebuild_if_needed(factory, env);
        self.line_numbers.rebuild_if_needed(factory, env);
    }

    /// Updates the line numbers column width.
    pub fn update_line_numbers_width(
        &mut self,
        data: &AppState,
        factory: &mut PietText,
        env: &Env,
    ) {
        // there is no obvious way to get the width that a text is going to occupy.
        // So we can build a TextLayout and retreive its size instead.
        let mut numbers =
            data.editor.buffer().measure::<LinesMetric>() + data.editor.lines_displayed.get();
        let mut string = String::with_capacity(10);
        while numbers != 0 {
            string.push('_');
            numbers /= 10;
        }

        self.line_numbers_width = factory
            .new_text_layout(string)
            .font(FONT.family.clone(), FONT.size)
            .build()
            .unwrap()
            .size()
            .width;
        self.line_numbers.set_wrap_width(self.line_numbers_width);
        self.line_numbers.rebuild_if_needed(factory, env);
    }

    /// Updates the line height.
    pub fn update_line_height(&mut self, factory: &mut PietText) {
        self.line_height = factory
            .new_text_layout("")
            .font(FONT.family.clone(), FONT.size)
            .build()
            .unwrap()
            .line_metric(0)
            .unwrap()
            .height;
    }

    /// Updates the syntax highlighting with the new language.
    pub fn update_syntax(&mut self, data: &AppState, sink: ExtEventSink) {
        // don't try to highlight big files or plain text files.
        if data.editor.buffer().measure::<LinesMetric>() > 100_000
            || data.editor.language == "Plain Text"
        {
            self.highlight_cache = None;
        } else {
            self.syntax = settings::PS
                .find_syntax_by_name(&data.editor.language)
                .unwrap();
            self.highlight_cache =
                Some(HighlightCache::new(self.syntax, data.editor.buffer(), sink));
        }
    }
}

impl Widget<AppState> for Ne2Editor {
    fn event(
        &mut self,
        ctx: &mut druid::EventCtx,
        event: &druid::Event,
        data: &mut AppState,
        env: &Env,
    ) {
        match event {
            Event::Command(cmd) => {
                if let Some(&first_highlighted_line) = cmd.get(NEW_HIGHLIGHTING) {
                    // update only the textview if the new highlighted lines are in the viewport
                    if data.editor.first_line() <= first_highlighted_line + LINES_PER_COMMAND
                        && first_highlighted_line <= data.editor.last_line()
                    {
                        self.update_textview(data, ctx.text(), env);
                    }
                    ctx.set_handled();
                }

                if let Some(path) = cmd.get(OPEN_FILE_IN_EDITOR) {
                    data.open_file(path);
                    ctx.set_handled();
                }
            }

            Event::KeyDown(key) => {
                data.vim
                    .handle_key(key.key.clone(), key.mods, &mut data.editor, ctx);
                data.editor.update_line_offset();
                data.editor.update_cursor_offset();
                data.editor.update_cursor_over_size();
                ctx.set_handled();
            }

            Event::MouseDown(_) => {
                ctx.request_focus();
            }

            Event::Wheel(mouse_event) => {
                data.editor.line_offset = (data.editor.line_offset
                    + mouse_event.wheel_delta.y / 120.)
                    .clamp(0., data.editor.buffer().measure::<LinesMetric>() as f64);
                ctx.set_handled();
            }

            Event::Paste(item) => if let Some(_string) = item.get_string() {},
            _ => (),
        }
    }

    fn lifecycle(
        &mut self,
        ctx: &mut druid::LifeCycleCtx,
        event: &druid::LifeCycle,
        data: &AppState,
        env: &Env,
    ) {
        match event {
            LifeCycle::WidgetAdded => {
                ctx.register_for_focus();

                self.update_line_height(ctx.text());
                self.update_normal_cursor(ctx.text());
                self.update_syntax(data, ctx.get_external_handle());
                self.update_textview(data, ctx.text(), env);
            }

            LifeCycle::Size(size) => {
                data.editor
                    .update_lines_displayed(size.height, self.line_height);
                self.update_line_numbers_width(data, ctx.text(), env);
            }
            _ => (),
        }
    }

    fn update(
        &mut self,
        ctx: &mut druid::UpdateCtx,
        old_data: &AppState,
        data: &AppState,
        env: &druid::Env,
    ) {
        let mut should_update_textview = false;

        // check if an edit was made
        if !data.editor.delta.same(&old_data.editor.delta) {
            should_update_textview = true;

            // invalidate the syntax highlighting starting from this line
            if let Some(highlight_cache) = &mut self.highlight_cache {
                highlight_cache.invalidate(
                    data.editor.delta.first_modified_char(),
                    data.editor.buffer(),
                );
            }

            // check if we need to change the size of the line numbers column
            let old_buffer = data.editor.delta.old_buffer(&data.editor.engine);
            if data.editor.buffer().measure::<LinesMetric>() != old_buffer.measure::<LinesMetric>()
            {
                self.update_line_numbers_width(data, ctx.text(), env);
            }
        }

        if !data.editor.language.same(&old_data.editor.language) {
            self.update_syntax(data, ctx.get_external_handle());
        }

        if !data.editor.file.same(&old_data.editor.file) {
            if let Some(highlight_cache) = &mut self.highlight_cache {
                highlight_cache.clear(data.editor.buffer());
            }

            self.update_line_numbers_width(data, ctx.text(), env);
            should_update_textview = true;
        }

        // some scroll happened
        if !data.editor.line_offset.same(&old_data.editor.line_offset) {
            should_update_textview = true;
        }

        if data.editor.lines_displayed != old_data.editor.lines_displayed {
            should_update_textview = true;
        }

        if !data.editor.cursor.same(&old_data.editor.cursor) {
            ctx.request_paint();
        }

        if should_update_textview {
            self.update_textview(data, ctx.text(), env);
            ctx.request_paint();
        }
    }

    fn layout(
        &mut self,
        ctx: &mut druid::LayoutCtx,
        bc: &druid::BoxConstraints,
        _data: &AppState,
        env: &druid::Env,
    ) -> druid::Size {
        self.text_view.rebuild_if_needed(ctx.text(), env);
        self.line_numbers.rebuild_if_needed(ctx.text(), env);

        let text_view_size = self.text_view.size();
        let line_numbers_size = self.line_numbers.size();

        let size = bc.constrain(Size::new(
            text_view_size.width + line_numbers_size.width + 64.,
            line_numbers_size.height,
        ));

        let text_metrics = self.text_view.layout_metrics();
        let height = text_metrics.size.height;

        let bottom_padding = (size.height - height) / 2.0;
        let baseline_off = bottom_padding + (height - text_metrics.first_baseline);

        ctx.set_baseline_offset(baseline_off);
        size
    }

    fn paint(&mut self, ctx: &mut druid::PaintCtx, data: &AppState, _env: &druid::Env) {
        let background_color = THEME.settings.background.unwrap().to_color();
        let cursor_color = THEME.settings.caret.unwrap().to_color();
        let line_highlight_color = THEME.settings.line_highlight.unwrap().to_color();
        let line_numbers_color = THEME
            .settings
            .gutter
            .unwrap_or_else(|| THEME.settings.background.unwrap())
            .to_color();

        let size = ctx.size();
        let text_pos = Point::new(self.line_numbers_width + 32., 0.);

        // background
        let clip_rect = size.to_rect();
        ctx.fill(clip_rect, &background_color);

        // draw the current line highlight and cursor if needed
        ctx.with_save(|rc| {
            rc.clip(clip_rect);

            let cursor_line = data.editor.cursor.y as isize - data.editor.line_offset as isize;
            if cursor_line < 0 || cursor_line >= data.editor.lines_displayed.get() as isize {
                return;
            }

            let line_y = match self
                .text_view
                .layout()
                .unwrap()
                .line_metric(cursor_line as usize)
            {
                Some(line_y) => line_y,
                None => return,
            };

            let line_rect = Size::new(size.width, line_y.height)
                .to_rect()
                .with_origin((0., line_y.y_offset));
            rc.fill(line_rect, &line_highlight_color);

            let cursor_offset = line_y.start_offset + data.editor.cursor.x_offset.unwrap();

            rc.transform(Affine::translate(text_pos.to_vec2()));

            // draw the thicc cursor
            if data.editor.cursor.mode == CursorMode::Over {
                let cursor_shape = if data.editor.cursor.x.is_some() {
                    self.text_view
                        .rects_for_range(
                            cursor_offset..cursor_offset + data.editor.cursor.over_size.unwrap(),
                        )
                        .into_iter()
                        .next()
                        .unwrap()
                } else {
                    let pt = self.text_view.point_for_text_position(line_y.start_offset);
                    rc.transform(Affine::translate(pt.to_vec2()));
                    rc.transform(Affine::translate((0., -line_y.baseline)));
                    self.normal_cursor_rect
                };
                rc.fill(cursor_shape, &cursor_color.with_alpha(0.5));

            // in insert mode draw a simple cursor
            } else {
                let cursor_line = self.text_view.cursor_line_for_text_position(cursor_offset);
                rc.stroke(cursor_line, &cursor_color, 2.);
            }
        });

        ctx.with_save(|rc| {
            rc.clip(clip_rect);

            let line_rect = Size::new(self.line_numbers_width + 16., size.height).to_rect();
            rc.fill(line_rect, &line_numbers_color);

            // line numbers
            self.line_numbers.draw(rc, Point::new(8., 0.));

            // text
            self.text_view.draw(rc, text_pos);
        });
    }
}

/// Builds the text editor
pub fn editor_builder() -> impl Widget<AppState> {
    Ne2Editor::new().with_id(EDITOR_ID)
}
