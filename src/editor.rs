//! All the editor logic, from the text editing to the UI rendering.

use crate::{
    app::AppState,
    file_explorer::OPEN_FILE_IN_EDITOR,
    highlight::{HighlightCache, LINES_PER_COMMAND, NEW_HIGHLIGHTING},
    lsp::{get_lsp_server, LspServer},
    settings::{self, FONT, THEME},
    utils::{LineEnding, ToColor},
    vim::{VimMotionKind, VimWord},
};
use druid::{
    piet::{PietText, Text, TextLayout as PietTextLayout, TextLayoutBuilder},
    text::{Attribute, RichText},
    Affine, Color, Data, Env, Event, ExtEventSink, FileInfo, FontWeight, Lens, LifeCycle, Point,
    Rect, RenderContext, Selector, Size, TextAlignment, TextLayout, Widget, WidgetExt, WidgetId,
};
use unicode_segmentation::UnicodeSegmentation;

use std::{
    borrow::Cow,
    cell::Cell,
    fs, iter,
    ops::Range,
    path::PathBuf,
    sync::{Arc, Mutex},
};
use syntect::{highlighting::FontStyle, parsing::SyntaxReference};
use xi_rope::{
    delta,
    engine::{Engine, RevId},
    tree, LinesMetric, Rope, RopeDelta, RopeInfo,
};

/// The command sent when a directory is open.
pub const OPEN_DIRECTORY: Selector<FileInfo> = Selector::new("ne2.editor.open_directory");

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
    /// The programming language of the current file.
    pub language: String,
    /// The path of the file being edited. None if the file hasn't been saved to the disk yet.
    pub file: Option<Arc<PathBuf>>,
    /// The attached LSP server if there is one.
    #[data(ignore)]
    pub lsp: Option<Arc<Mutex<LspServer>>>,
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
    pub fn new(start_path: Option<PathBuf>) -> Self {
        // we store first the buffer in a string before passing it to the rope.
        let string = if let Some(path) = &start_path {
            fs::read_to_string(path).expect("Invalid file path!")
        } else {
            String::new()
        };

        let first_line = string.lines().next();
        let x = if string.is_empty() { None } else { Some(0) };

        let file = start_path.clone().map(Arc::new);

        // get the syntax from the file extension or the first line of the file
        let language = EditorState::find_language(start_path, first_line);

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

        let rope = Rope::from(string);
        let over_param = rope.next_grapheme_offset(0).unwrap_or_default();
        let engine = Engine::new(rope);

        Self {
            cursor: Cursor {
                x,
                wanted_x: 0,
                y: 0,
                mode: CursorMode::Over(over_param),
            },
            line_offset: 0.,
            lines_displayed: Cell::new(0),
            delta: DeltaState::new_empty(engine.get_head_rev_id()),
            engine: Arc::new(Mutex::new(engine)),
            file,
            line_ending,
            lsp: None,
            language,
        }
    }

    /// Finds the file's language when given a path and a first line.
    pub fn find_language(path: Option<PathBuf>, first_line: Option<&str>) -> String {
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

    /// Downloads the LSP server of the current language if the user asked it.
    pub fn download_lsp_server(&mut self, release_id: usize, download_url: String) {
        let lsp_server = self.lsp.as_mut().unwrap();
        lsp_server
            .lock()
            .unwrap()
            .download(release_id, download_url);
    }

    /// Updates the language.
    pub fn update_language(&mut self) {
        let buffer = self.buffer();
        if buffer.measure::<LinesMetric>() >= 1 {
            let first_line_offset = buffer.offset_of_line(1);
            let slice = buffer.slice_to_cow(..first_line_offset);
            self.language = EditorState::find_language(
                self.file.as_ref().map(|a| a.to_path_buf()),
                Some(&slice),
            );
        } else {
            self.language =
                EditorState::find_language(self.file.as_ref().map(|a| a.to_path_buf()), None);
        };

        // look for this language LSP.
        self.lsp = get_lsp_server(&self.language).map(|lsp| {
            lsp.check_update();
            Arc::new(Mutex::new(lsp))
        });
    }

    /// Updates the cursor region in case of an Over cursor.
    pub fn update_cursor_region(&mut self) {
        if self.cursor.mode == CursorMode::Boundary {
            return;
        }

        let buffer = self.buffer();
        let line_offset = buffer.offset_of_line(self.cursor.y);

        let size = match self.cursor.x {
            None => 0,
            Some(x) => {
                let mut it = GraphemeIterator::new(&buffer, line_offset);
                let start_offset = if x == 0 {
                    line_offset
                } else {
                    it.nth(x - 1).unwrap()
                };
                it.next().unwrap() - start_offset
            }
        };

        self.cursor.mode = CursorMode::Over(size);
    }

    /// Updates the cursor's x coordinate to None if it's on a empty line.
    pub fn update_empty_line(&mut self) {
        if self.cursor.x.is_none() {
            return;
        }

        let buffer = self.buffer();
        let line_offset = buffer.offset_of_line(self.cursor.y);
        let line = buffer.lines(line_offset..).next();

        if line.unwrap_or_default().is_empty() {
            self.cursor.x = None;
        }
    }

    /// Updates the line offset if needed.
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
        let mut engine = self.engine.lock().unwrap();
        let rev = engine.get_head_rev_id().token();

        let mut builder = delta::Builder::new(engine.get_head().len());
        builder.delete(range);
        let delta = builder.build();

        engine.edit_rev(0, 0, rev, delta.clone());

        self.delta = DeltaState::new(delta, engine.get_head_rev_id());
    }

    /// Edits the specified range of the buffer while registering the edit in the engine.
    pub fn edit(&mut self, range: Range<usize>, text: &str) {
        let mut engine = self.engine.lock().unwrap();
        let rev = engine.get_head_rev_id().token();

        let mut builder = delta::Builder::new(engine.get_head().len());
        builder.replace(range, text.into());
        let delta = builder.build();

        engine.edit_rev(0, 0, rev, delta.clone());

        self.delta = DeltaState::new(delta, engine.get_head_rev_id());
    }

    /// Gets the x offset of the cursor.
    pub fn cursor_x_offset(&self) -> usize {
        let buffer = self.buffer();
        let line_offset = buffer.offset_of_line(self.cursor.y);

        let x = match self.cursor.x {
            Some(0) | None => return 0,
            Some(x) => x,
        };

        GraphemeIterator::new(&buffer, line_offset)
            .nth(x - 1)
            .unwrap()
            - line_offset
    }

    /// Gets the offset behind the cursor.
    pub fn cursor_offset(&self) -> usize {
        let buffer = self.buffer();
        let line_offset = buffer.offset_of_line(self.cursor.y);

        let x = match self.cursor.x {
            Some(0) | None => return line_offset,
            Some(x) => x,
        };

        GraphemeIterator::new(&buffer, line_offset)
            .nth(x - 1)
            .unwrap()
    }

    /// Infers the text range affected by vim primitives
    pub fn infer_range(&self, motion: VimMotionKind, multiplier: usize) -> Range<usize> {
        let start;
        let end;
        match motion {
            VimMotionKind::Line => {
                start = self.buffer().offset_of_line(self.cursor.y);
                let buffer_len = self.buffer().measure::<LinesMetric>();

                if self.cursor.y + multiplier > buffer_len {
                    end = self.buffer().offset_of_line(buffer_len + 1);
                } else {
                    end = self.buffer().offset_of_line(self.cursor.y + multiplier);
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
            self.cursor.y -= 1;
            self.cursor_to_eol();
            self.move_cursor(Direction::Left);
            self.delete_line_ending(self.cursor.y);

        // delete one character on the current line
        } else {
            let cursor_offset = self.cursor_offset();
            self.cursor.x = Some(cursor_x - 1);
            self.cursor.wanted_x = cursor_x - 1;

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

        let byte_offset = self.buffer().offset_of_line(line_idx) + line.len() - line_ending_len;
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

    /// Moves the cursor to the end of the line. This means on the last character
    /// if the cursor mode is set on Over, or after the last character if the cursor
    /// mode is set on Boundary.
    pub fn cursor_to_eol(&mut self) {
        if let Some(x) = self.last_grapheme_for_line(self.cursor.y) {
            if self.cursor.mode == CursorMode::Boundary {
                self.cursor.x = Some(x + 1);
                self.cursor.wanted_x = x + 1;
            } else {
                self.cursor.x = Some(x);
                self.cursor.wanted_x = x;
            }
        } else {
            self.cursor.x = None;
            self.cursor.wanted_x = 0;
        }
    }

    /// Moves the cursor to the start of the line.
    #[inline]
    pub fn cursor_to_sol(&mut self) {
        self.cursor.x = Some(0);
        self.cursor.wanted_x = 0;
    }

    /// Moves the cursor to the specified line.
    #[inline]
    pub fn cursor_to_line(&mut self, line_idx: usize) {
        self.cursor.y = line_idx;
        self.cursor_to_sol();
    }

    /// Inserts a newline at the cursor position and matches the indentation level.
    pub fn cursor_to_newline(&mut self) {
        let indent = self.insert_newline(self.cursor_offset());
        self.cursor.y += 1;
        self.cursor.x = Some(indent);
        self.cursor.wanted_x = indent;
    }

    /// Moves the cursor to the specified byte offset.
    pub fn cursor_to_byte(&mut self, byte_offset: usize) {
        let buffer = self.buffer();

        if buffer.is_empty() {
            return;
        }

        self.cursor.y = buffer.line_of_offset(byte_offset);
        let line_start = buffer.offset_of_line(self.cursor.y);
        let x = GraphemeIterator::new(&buffer, line_start)
            .take_while(|&offset| offset <= byte_offset)
            .count();
        self.cursor.x = Some(x);
        self.cursor.wanted_x = x;
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
                self.cursor.y -= 1;
                self.cursor.x = self
                    .last_grapheme_for_line(self.cursor.y)
                    .map(|last_char| self.cursor.wanted_x.min(last_char - boundary));
            }
            Direction::Down if self.cursor.y < self.buffer().measure::<LinesMetric>() => {
                self.cursor.y += 1;
                self.cursor.x = self
                    .last_grapheme_for_line(self.cursor.y)
                    .map(|last_char| self.cursor.wanted_x.min(last_char - boundary));
            }
            Direction::Left if self.cursor.x > Some(0) => {
                let new_x = self.cursor.x.unwrap() - 1;
                self.cursor.x = Some(new_x);
                self.cursor.wanted_x = new_x;
            }
            Direction::Right => {
                if let Some(x) = self.cursor.x {
                    let last_char = self.last_grapheme_for_line(self.cursor.y);

                    // allow positionning the cursor at the end of the line in insert mode
                    if Some(x + boundary) < last_char {
                        self.cursor.x = Some(x + 1);
                        self.cursor.wanted_x = x + 1;
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
        self.cursor.wanted_x = x;
        self.cursor.x = Some(x);
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
        let file = match &self.file {
            Some(file) => file,
            None => {
                return;
            }
        };

        let buffer = self.buffer();
        fs::write(file.as_os_str(), buffer.slice_to_cow(..).as_bytes())
            .expect("Could not write file to disk!");
    }
}

/// An unique edit state of a Rope, identified by its RevId.
/// This is useful to easely keep track of modifications with druid.
#[derive(Clone)]
pub struct DeltaState {
    /// The delta.
    delta: Option<RopeDelta>,
    /// The revision using this delta.
    pub rev: RevId,
}

impl DeltaState {
    /// Creates a new delta state.
    pub fn new(delta: RopeDelta, rev: RevId) -> Self {
        Self {
            delta: Some(delta),
            rev,
        }
    }

    /// Creates a new empty delta state.
    pub fn new_empty(rev: RevId) -> Self {
        Self { delta: None, rev }
    }

    /// Returns the rope associated to this state.
    #[inline]
    pub fn rope(&self, engine: &Arc<Mutex<Engine>>) -> Rope {
        engine
            .lock()
            .unwrap()
            .get_rev(self.rev.token())
            .unwrap_or_default()
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
}

impl Data for DeltaState {
    fn same(&self, other: &Self) -> bool {
        self.rev == other.rev && self.delta.is_some() == other.delta.is_some()
    }
}

/// A cursor.
#[derive(Clone, Data, Debug)]
pub struct Cursor {
    /// The wanted x coordinate in utf-8 graphemes.
    pub wanted_x: usize,
    /// The real x coordinate in utf-8 graphemes, if the line is too short to use the wanted x coordinate.
    pub x: Option<usize>,
    /// The y coordinate in lines.
    pub y: usize,
    /// The mode of the cursor.
    pub mode: CursorMode,
}

/// The mode of a cursor. Specifically, how a cursor interacts with the underlaying characters.
#[derive(Clone, Data, Debug, PartialEq)]
pub enum CursorMode {
    /// A cursor right over a character. This is how a cursor is represented in vim's normal mode.
    /// What we visually identify as a character is not always of the same size.
    /// This is why we have an usize parameter, tracking the grapheme size.
    /// If the cursor is not on any character but on an empty line, this should be 0.
    /// ```text
    /// Cursor pos   Over parameter
    ///      --------------  
    ///     |       é      |
    ///      --------------
    /// ```
    Over(usize),
    /// A cursor at a character boundary (right before or right after a character). This is how a cursor is
    /// represented in vim's insert mode.
    /// ```text
    ///                Cursor pos
    ///             é      |
    /// ```
    Boundary,
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
    pub fn update_textview(&mut self, data: &AppState) {
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
    }

    /// Updates the line numbers column width.
    pub fn update_line_numbers_width(&mut self, text: &mut PietText, data: &AppState) {
        // there is no obvious way to get the width that
        // a text is going to occupy. So we can build
        // a TextLayout and retreive its size instead.
        let mut numbers = data.editor.buffer().measure::<LinesMetric>() + 100;
        let mut string = String::with_capacity(10);
        while numbers != 0 {
            string.push('_');
            numbers /= 10;
        }

        self.line_numbers_width = text
            .new_text_layout(string)
            .font(FONT.family.clone(), FONT.size)
            .build()
            .unwrap()
            .size()
            .width;
        self.line_numbers.set_wrap_width(self.line_numbers_width);
    }

    /// Updates the line height.
    pub fn update_line_height(&mut self, text: &mut PietText) {
        self.line_height = text
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
        _env: &Env,
    ) {
        match event {
            Event::Command(command) => {
                if let Some(&first_highlighted_line) = command.get(NEW_HIGHLIGHTING) {
                    // update only the textview if the new highlighted lines are in the viewport
                    if data.editor.first_line() <= first_highlighted_line + LINES_PER_COMMAND
                        && first_highlighted_line <= data.editor.last_line()
                    {
                        self.update_textview(data);
                        ctx.request_layout();
                    }
                    ctx.set_handled();
                }

                if let Some(path) = command.get(OPEN_FILE_IN_EDITOR) {
                    data.open_file(path);
                    ctx.set_handled();
                }
            }

            Event::KeyDown(key) => {
                let old_delta = data.editor.delta.clone();
                let old_cursor = data.editor.cursor.clone();

                data.vim
                    .handle_key(key.key.clone(), key.mods, &mut data.editor, ctx);
                data.editor.update_line_offset();

                if !data.editor.delta.same(&old_delta) || !data.editor.cursor.same(&old_cursor) {
                    data.editor.update_empty_line();
                    data.editor.update_cursor_region();
                }

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
        _env: &Env,
    ) {
        match event {
            LifeCycle::WidgetAdded => {
                ctx.register_for_focus();

                self.update_line_height(ctx.text());
                self.update_line_numbers_width(ctx.text(), data);
                self.update_normal_cursor(ctx.text());
                self.update_syntax(data, ctx.get_external_handle());
                self.update_textview(data);

                data.editor
                    .update_lines_displayed(ctx.size().height, self.line_height);
                // #[allow(deprecated)]
                // if let Some(lsp) = &data.editor.lsp {
                //     lsp.lock().unwrap().send(
                //         Initialize::METHOD,
                //         InitializeParams {
                //             process_id: Some(process::id()),
                //             root_path: None,
                //             root_uri: None,
                //             initialization_options: None,
                //             capabilities: ClientCapabilities::default(),
                //             trace: None,
                //             workspace_folders: None,
                //             client_info: None,
                //             locale: None,
                //         },
                //     );
                // }
            }

            LifeCycle::Size(size) => data
                .editor
                .update_lines_displayed(size.height, self.line_height),

            _ => (),
        }
    }

    fn update(
        &mut self,
        ctx: &mut druid::UpdateCtx,
        old_data: &AppState,
        data: &AppState,
        _env: &druid::Env,
    ) {
        let mut should_update_textview = false;

        // check if an edit was made
        if !data.editor.delta.same(&old_data.editor.delta) {
            // invalidate the syntax highlighting starting from this line
            if let Some(highlight_cache) = &mut self.highlight_cache {
                highlight_cache.invalidate(
                    data.editor.delta.first_modified_char(),
                    data.editor.buffer(),
                );
            }

            should_update_textview = true;

            // check if we need to change the size of the line numbers column
            // we could ideally do this less often, but length comparisons can
            // be more costly sometimes.
            self.update_line_numbers_width(ctx.text(), data);
        }

        if !data.editor.language.same(&old_data.editor.language) {
            self.update_syntax(data, ctx.get_external_handle());
        }

        if !data.editor.file.same(&old_data.editor.file) {
            if let Some(highlight_cache) = &mut self.highlight_cache {
                highlight_cache.clear(data.editor.buffer());
            }

            self.update_line_numbers_width(ctx.text(), data);
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

        // updating text requires a layout rebuild
        if should_update_textview {
            self.update_textview(data);
            ctx.request_layout();
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
            if cursor_line < 0 {
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

            let cursor_char = line_y.start_offset + data.editor.cursor_x_offset();

            rc.transform(Affine::translate(text_pos.to_vec2()));

            // draw the thicc cursor
            if let CursorMode::Over(grapheme_size) = data.editor.cursor.mode {
                let cursor_shape = if data.editor.cursor.x.is_some() {
                    self.text_view
                        .rects_for_range(cursor_char..cursor_char + grapheme_size)
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
                let cursor_line = self.text_view.cursor_line_for_text_position(cursor_char);
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
