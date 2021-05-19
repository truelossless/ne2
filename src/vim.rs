//! Vim-like motions.

use crate::{
    app::SAVE_FILE_AS_AND_EXIT,
    editor::{Direction, EditorState, OPEN_DIRECTORY},
    utils::ActionKey,
};
use druid::{
    commands::{SHOW_OPEN_PANEL, SHOW_SAVE_PANEL},
    keyboard_types::Key,
    Application, Data, EventCtx, FileDialogOptions, Modifiers,
};
use std::{char, sync::Arc};

/// Characters that should be matched with their closing
/// counterpart upon insertion.
const AUTOCLOSING_CHARACTERS: [(char, char); 3] = [('(', ')'), ('[', ']'), ('{', '}')];

/// A vim action.
#[derive(Clone, Data, Default)]
pub struct VimAction {
    /// The kind of action.
    kind: VimActionKind,
    /// Whether or not this action is done.
    is_done: bool,
    /// The underlying motion.
    motion: VimMotion,
    /// All the characters in the action.
    chars: Arc<String>,
    /// The multiplier, in its text form.
    numbers: Arc<String>,
    /// The cached multiplier.
    multiplier: Option<usize>,
}

impl VimAction {
    /// Returns the multiplier of this action.
    pub fn multiplier(&mut self) -> usize {
        self.multiplier.unwrap_or_else(|| {
            let multiplier = self.numbers.parse().unwrap_or(1);
            self.multiplier = Some(multiplier);
            multiplier
        })
    }

    /// Returns the motion kind behind this action.
    pub fn motion(&mut self) -> VimMotionKind {
        self.motion.get(self.kind.clone())
    }

    /// Returns the motion multiplier behind this action.
    pub fn motion_multiplier(&mut self) -> usize {
        self.motion.multiplier()
    }

    /// Checks if the action is done.
    pub fn is_done(&self) -> bool {
        self.is_done
    }

    /// Adds a char to the action.
    pub fn add_char(&mut self, c: char) {
        let chars = Arc::make_mut(&mut self.chars);
        chars.push(c);

        if self.kind == VimActionKind::None {
            if c.is_ascii_digit() {
                let numbers = Arc::make_mut(&mut self.numbers);
                numbers.push(c);
            } else if let Some(kind) = VimActionKind::try_new(&self.chars) {
                self.kind = kind;
                if self.kind.is_immediate() {
                    self.is_done = true;
                }
            }
        } else {
            self.motion.add_char(c, self.kind.clone());

            if self.motion.is_done() {
                self.is_done = true;
            }
        }
    }
}

impl ToString for VimAction {
    fn to_string(&self) -> String {
        self.chars.to_string()
    }
}

/// A vim motion such as 5w.
#[derive(Default, Clone, Data)]
pub struct VimMotion {
    /// The multiplier, in its text form.
    numbers: Arc<String>,
    /// The cached multiplier.
    multiplier: Option<usize>,
    /// Every char belonging to this motion.
    chars: Arc<String>,
    /// Whether or not this motion is done.
    is_done: bool,
    /// The cached motion kind.
    kind: Option<VimMotionKind>,
}

impl VimMotion {
    /// Returns the multiplier of this motion.
    pub fn multiplier(&mut self) -> usize {
        self.multiplier.unwrap_or_else(|| {
            let multiplier = self.numbers.parse().unwrap_or(1);
            self.multiplier = Some(multiplier);
            multiplier
        })
    }

    /// Adds a char to the motion with respect to the context of the original action.
    pub fn add_char(&mut self, c: char, ctx: VimActionKind) {
        if c.is_ascii_digit() {
            let numbers = Arc::make_mut(&mut self.numbers);
            numbers.push(c);
        } else {
            if c != 'i' {
                self.is_done = true;
            }

            let c = if c == 'c' && ctx == VimActionKind::Change
                || c == 'd' && ctx == VimActionKind::Delete
            {
                '_'
            } else {
                c
            };

            let chars = Arc::make_mut(&mut self.chars);
            chars.push(c);
        }
    }

    /// Checks if the motion is done.
    pub fn is_done(&self) -> bool {
        self.is_done
    }

    /// Gets the motion kind with respect to the context of the original action.
    pub fn get(&mut self, ctx: VimActionKind) -> VimMotionKind {
        self.kind.clone().unwrap_or_else(|| {
            let motion_kind = match self.chars.as_str() {
                "w" => VimMotionKind::WordStartForwards,
                "W" => VimMotionKind::FullWordStartForwards,
                "iw" => VimMotionKind::InnerWord,
                "iW" => VimMotionKind::FullInnerWord,
                "b" => VimMotionKind::WordStartBackwards,
                "B" => VimMotionKind::FullWordStartBackwards,
                "e" => VimMotionKind::WordEndForwards,
                "E" => VimMotionKind::FullWordEndForwards,
                "h" => VimMotionKind::CharacterBackwards,
                "j" if ctx == VimActionKind::Change || ctx == VimActionKind::Delete => {
                    VimMotionKind::LineDownwards
                }
                "j" => VimMotionKind::CharacterUpwards,
                "k" if ctx == VimActionKind::Change || ctx == VimActionKind::Delete => {
                    VimMotionKind::LineUpwards
                }
                "k" => VimMotionKind::CharacterDownwards,
                "l" => VimMotionKind::CharacterForwards,
                "_" => VimMotionKind::Line,
                _ => unimplemented!(),
            };

            self.kind = Some(motion_kind.clone());
            motion_kind
        })
    }
}

/// All kinds of vim motions.
/// A word matches \[_a-zA-Z0-9\]+. WORDS are separated by space.
#[derive(Clone, Data, PartialEq)]
pub enum VimMotionKind {
    /// The next character
    CharacterForwards,
    /// The previous character
    CharacterBackwards,
    /// The upward character
    CharacterUpwards,
    /// The downward character
    CharacterDownwards,
    /// The start of the forward word.
    WordStartForwards,
    /// The start of the forward WORD.
    FullWordStartForwards,
    /// The end of the forward word.
    WordEndForwards,
    /// The end of the forward WORD.
    FullWordEndForwards,
    /// The end of the backward word.
    WordStartBackwards,
    /// The end of the backward WORD.
    FullWordStartBackwards,
    /// The current word.
    InnerWord,
    /// The current WORD.
    FullInnerWord,
    /// The upward line.
    LineUpwards,
    /// The downward line.
    LineDownwards,
    /// The current line
    Line,
}

/// All kinds of vim actions.
#[derive(Clone, Data, PartialEq)]
pub enum VimActionKind {
    /// No current action.
    None,
    /// Enters insert mode.
    Insert,
    /// Enters insert mode and move the cursor to the right.
    Append,
    /// Enters visual mode.
    Visual,
    /// Enters visual line mode.
    VisualLine,
    /// Opens a line after the current line.
    AppendLine,
    /// Opens a line before the current line.
    PrependLine,
    /// Changes text.
    Change,
    /// Deletes text.
    Delete,
    /// Deletes a character.
    DeleteChar,
    /// Replaces a character.
    ReplaceChar(char),
    /// Moves somewhere
    Move(VimMotionKind),
    /// Repeats the latest action.
    Repeat,
}

impl Default for VimActionKind {
    fn default() -> Self {
        VimActionKind::None
    }
}

impl VimActionKind {
    /// Tries to create a new VimActionKind based on the first chars.
    /// This returns None if we need more context to create the action.
    pub fn try_new(s: &str) -> Option<Self> {
        Some(match s.chars().collect::<Vec<_>>()[..] {
            ['i'] => VimActionKind::Insert,
            ['a'] => VimActionKind::Append,
            ['o'] => VimActionKind::AppendLine,
            ['O'] => VimActionKind::PrependLine,
            ['d'] => VimActionKind::Delete,
            ['c'] => VimActionKind::Change,
            ['r', next_char] => VimActionKind::ReplaceChar(next_char),
            ['r'] => return None,
            ['x'] => VimActionKind::DeleteChar,
            ['w'] => VimActionKind::Move(VimMotionKind::WordStartForwards),
            ['W'] => VimActionKind::Move(VimMotionKind::FullWordStartForwards),
            ['e'] => VimActionKind::Move(VimMotionKind::WordEndForwards),
            ['E'] => VimActionKind::Move(VimMotionKind::FullWordEndForwards),
            ['b'] => VimActionKind::Move(VimMotionKind::WordStartForwards),
            ['B'] => VimActionKind::Move(VimMotionKind::WordStartBackwards),
            ['h'] => VimActionKind::Move(VimMotionKind::CharacterBackwards),
            ['j'] => VimActionKind::Move(VimMotionKind::CharacterDownwards),
            ['k'] => VimActionKind::Move(VimMotionKind::CharacterUpwards),
            ['l'] => VimActionKind::Move(VimMotionKind::CharacterForwards),
            ['.'] => VimActionKind::Repeat,
            _ => VimActionKind::None,
        })
    }

    /// Returns true if the action doesn't need a motion to take effect.
    pub fn is_immediate(&self) -> bool {
        matches!(
            &self,
            VimActionKind::AppendLine
                | VimActionKind::PrependLine
                | VimActionKind::Insert
                | VimActionKind::Append
                | VimActionKind::Repeat
                | VimActionKind::Move(_)
                | VimActionKind::ReplaceChar(_)
                | VimActionKind::DeleteChar
        )
    }
}

/// A way of keeping track of a vim word.
pub struct VimWord {
    /// Whether or not we have a "true" word or we're on spaces/special characters.
    /// This won't be set until we check for a first character.
    is_real_word: Option<bool>,
    /// Whether we're talking about a word or a WORD.
    is_full_word: bool,
}

impl VimWord {
    /// Keeps track of a vim word.
    pub fn regular() -> Self {
        Self {
            is_real_word: None,
            is_full_word: false,
        }
    }

    /// Keeps track of a vim WORD.
    pub fn full() -> Self {
        Self {
            is_real_word: None,
            is_full_word: true,
        }
    }

    /// Checks if the grapheme belongs to a word or is a special character.
    fn is_word_grapheme(&self, s: &str) -> bool {
        if self.is_full_word {
            true
        } else {
            ("A".."Z").contains(&s)
                || ("a".."z").contains(&s)
                || ("0".."9").contains(&s)
                || s == "_"
        }
    }

    /// Checks if a grapheme belongs to this word.
    pub fn check_grapheme(&mut self, s: &str) -> bool {
        if " \t\r\n".contains(s) {
            return false;
        }

        match self.is_real_word {
            Some(real_word) => self.is_word_grapheme(s) == real_word,
            None => {
                self.is_real_word = Some(self.is_word_grapheme(s));
                true
            }
        }
    }
}

/// The kind of a vim command.
#[derive(Clone, PartialEq, Data)]
pub enum VimCommandKind {
    /// No current command.
    None,
    /// Go to the specified line.
    GotoLine(usize),
    /// Save the current buffer.
    Save,
    /// Save the current buffer and quits ne2.
    SaveAndQuit,
    /// Quit ne2.
    Quit,
}

#[derive(Clone, Data, Default)]
/// A vim command such as :wq or :12.
pub struct VimCommand {
    /// The chars of the command.
    chars: String,
    /// The kind of the command.
    kind: Option<VimCommandKind>,
}

impl VimCommand {
    /// Adds a char to the command.
    #[inline]
    pub fn add_char(&mut self, c: char) {
        self.chars.push(c);
    }

    /// Removes the last char of the command.
    #[inline]
    pub fn pop_char(&mut self) {
        self.chars.pop();
    }

    /// Checks if the command is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.chars.is_empty()
    }

    /// Gets the kind of the command.
    pub fn get(&mut self) -> &VimCommandKind {
        if self.kind.is_none() {
            self.kind = Some(self.infer_command());
        }

        self.kind.as_mut().unwrap()
    }

    /// Infers the command from the chars.
    pub fn infer_command(&self) -> VimCommandKind {
        if self.chars.is_empty() {
            return VimCommandKind::None;
        }

        let mut char_iter = self.chars.chars().peekable();

        match char_iter.next().unwrap() {
            ':' => {
                let line_number_iter = char_iter.clone().take_while(|c| c.is_ascii_digit());
                let numbers: String = line_number_iter.collect();

                // if all the characters are line numbers, it's a GotoLine command
                if self.chars.len() == numbers.len() + 1 {
                    return VimCommandKind::GotoLine(numbers.parse().unwrap());
                }

                // if only some numbers match at the start of the command, the format is wrong
                if !numbers.is_empty() {
                    return VimCommandKind::None;
                }

                match &self.chars[1..] {
                    "w" => VimCommandKind::Save,
                    "wq" => VimCommandKind::SaveAndQuit,
                    "q" => VimCommandKind::Quit,
                    _ => VimCommandKind::None,
                }
            }
            _ => unreachable!(),
        }
    }
}

impl ToString for VimCommand {
    fn to_string(&self) -> String {
        self.chars.clone()
    }
}

/// A vim state keeping track of the context.
#[derive(Clone, Data, Default)]
pub struct VimState {
    /// The edit mode.
    pub mode: EditMode,
    /// The current action.
    pub action: VimAction,
    /// The last successful action.
    #[data(ignore)]
    last_action: VimAction,
    /// The current command if there is one.
    pub command: Option<VimCommand>,
}

impl VimState {
    /// Executes the current action.
    pub fn exec_action(&mut self, editor: &mut EditorState) {
        match self.action.kind.clone() {
            VimActionKind::Append => {
                self.insert_mode(editor);
                editor.move_cursor(Direction::Right);
            }

            VimActionKind::Insert => {
                self.insert_mode(editor);
            }

            VimActionKind::Change => {
                for _ in 0..self.action.multiplier() {
                    let range =
                        editor.infer_range(self.action.motion(), self.action.motion_multiplier());
                    editor.remove(range);
                }

                match self.action.motion() {
                    // replace correctly the cursor
                    VimMotionKind::Line => {
                        editor.cursor_to_sol();
                        editor.cursor_to_newline();
                        editor.move_cursor(Direction::Up);
                    }
                    _ => (),
                }

                self.insert_mode(editor);
            }

            VimActionKind::Delete => {
                for _ in 0..self.action.multiplier() {
                    let range =
                        editor.infer_range(self.action.motion(), self.action.motion_multiplier());
                    editor.remove(range);
                }

                match self.action.motion() {
                    // remove the blanks after the word
                    VimMotionKind::FullWordStartForwards | VimMotionKind::WordStartForwards => {
                        editor.remove_whitespace();
                    }

                    // place the cursor on the first non blank character of the next line
                    VimMotionKind::Line => {
                        editor.cursor_to_sol();
                        let offset = editor.cursor_offset();
                        let delta = editor.whitespace_length(offset);
                        editor.cursor_to_byte(offset + delta);
                    }
                    _ => (),
                }
            }

            VimActionKind::Move(motion) => {
                for _ in 0..self.action.multiplier() {
                    match motion {
                        VimMotionKind::CharacterBackwards => {
                            editor.move_cursor(Direction::Left);
                        }

                        VimMotionKind::CharacterForwards => {
                            editor.move_cursor(Direction::Right);
                        }

                        VimMotionKind::CharacterDownwards => {
                            editor.move_cursor(Direction::Down);
                        }

                        VimMotionKind::CharacterUpwards => {
                            editor.move_cursor(Direction::Up);
                        }

                        VimMotionKind::FullWordStartForwards | VimMotionKind::WordStartForwards => {
                            let mut range = editor.infer_range(motion.clone(), 1);
                            range.end += editor.blanks_length(range.end);
                            editor.cursor_to_byte(range.end);
                        }
                        _ => (),
                    }
                }
            }

            VimActionKind::ReplaceChar(new_char) => {
                for _ in 0..self.action.multiplier() {
                    let offset = editor.cursor_offset();
                    let over_size = editor.cursor_over_size();
                    let mut tmp = [0; 4];
                    editor.edit(offset..offset + over_size, new_char.encode_utf8(&mut tmp));
                }
            }

            VimActionKind::DeleteChar => {
                for _ in 0..self.action.multiplier() {
                    let offset = editor.cursor_offset();
                    let over_size = editor.cursor_over_size();
                    editor.remove(offset..offset + over_size);
                }
            }

            VimActionKind::PrependLine | VimActionKind::AppendLine => {
                let line_offset;

                if self.action.kind == VimActionKind::PrependLine {
                    line_offset = editor.cursor.y_offset.unwrap();
                } else {
                    line_offset = editor.buffer().offset_of_line(editor.cursor.y + 1);
                    editor.set_cursor_y(editor.cursor.y + 1);
                }

                let indent = editor.insert_newline(line_offset);
                editor.set_cursor_x(indent, true);
                self.insert_mode(editor);
            }

            VimActionKind::Repeat => {
                self.action = std::mem::take(&mut self.last_action);
                self.exec_action(editor);
            }

            _ => unimplemented!(),
        }
    }

    /// Executes the current command.
    pub fn exec_command(&mut self, editor: &mut EditorState, ctx: &mut EventCtx) {
        match &self.command.as_mut().unwrap().get() {
            VimCommandKind::GotoLine(l) => editor.cursor_to_line(*l - 1),
            VimCommandKind::Save => {
                if editor.file.is_some() {
                    editor.save_to_file();
                } else {
                    ctx.submit_command(SHOW_SAVE_PANEL.with(FileDialogOptions::new()));
                }
            }
            VimCommandKind::SaveAndQuit => {
                if editor.file.is_some() {
                    editor.save_to_file();
                    Application::global().quit();
                } else {
                    ctx.submit_command(
                        SHOW_SAVE_PANEL
                            .with(FileDialogOptions::new().accept_command(SAVE_FILE_AS_AND_EXIT)),
                    );
                }
            }
            VimCommandKind::Quit => {
                Application::global().quit();
            }
            VimCommandKind::None => (),
        }

        self.command = None;
    }

    /// Enters insert mode.
    pub fn insert_mode(&mut self, editor: &mut EditorState) {
        self.mode = EditMode::Insert;
        editor.cursor_to_boundary();
    }

    /// Enters normal mode.
    pub fn normal_mode(&mut self, editor: &mut EditorState) {
        if self.mode == EditMode::Insert {
            editor.move_cursor(Direction::Left);
        }

        self.mode = EditMode::Normal;
        editor.cursor_to_over();
        self.action = VimAction::default();
        self.command = None;
    }

    /// Handles a key press in the editor,
    pub fn handle_key(
        &mut self,
        mut key: Key,
        mods: Modifiers,
        editor: &mut EditorState,
        ctx: &mut EventCtx,
    ) {
        // transform the arrow keys into h,j,k,l in normal mode
        if self.mode == EditMode::Normal {
            key = match key {
                Key::ArrowLeft => Key::Character("h".to_owned()),
                Key::ArrowDown => Key::Character("j".to_owned()),
                Key::ArrowUp => Key::Character("k".to_owned()),
                Key::ArrowRight => Key::Character("l".to_owned()),
                _ => key,
            };
        }

        match key {
            // vim command in normal mode
            Key::Character(c)
                if c == ":" && self.mode == EditMode::Normal && self.command.is_none() =>
            {
                let mut command = VimCommand::default();
                command.add_char(':');
                self.command = Some(command);
            }

            // add a character to the command
            Key::Character(c) if self.command.is_some() && !mods.action_key() => {
                self.command
                    .as_mut()
                    .unwrap()
                    .add_char(c.chars().next().unwrap());
            }

            // remove a character from the command
            Key::Backspace if self.command.is_some() => {
                let command = self.command.as_mut().unwrap();
                command.pop_char();

                // exit the command if necessary
                if command.is_empty() {
                    self.command = None;
                }
            }

            // executing the command
            Key::Enter if !mods.action_key() && self.command.is_some() => {
                self.exec_command(editor, ctx);
            }

            // vim action in normal mode
            Key::Character(c) if !mods.action_key() && self.mode == EditMode::Normal => {
                self.action.add_char(c.chars().next().unwrap());

                if self.action.is_done() {
                    self.exec_action(editor);
                    self.last_action = std::mem::take(&mut self.action);
                }
            }

            // going back to normal mode
            Key::Escape => self.normal_mode(editor),
            Key::Character(c) if mods.ctrl() && c == "c" => self.normal_mode(editor),

            // adding a character
            Key::Character(c) if !mods.action_key() && self.mode == EditMode::Insert => {
                editor.insert(&c);

                let should_autoclose = AUTOCLOSING_CHARACTERS.iter().find_map(|(open, close)| {
                    if c.chars().next().unwrap() == *open {
                        Some(*close)
                    } else {
                        None
                    }
                });

                if let Some(close) = should_autoclose {
                    let mut tmp = [0; 4];
                    editor.insert(close.encode_utf8(&mut tmp));
                    editor.move_cursor(Direction::Left);
                }
            }

            // inserting a tab
            Key::Tab if !mods.action_key() && self.mode == EditMode::Insert => {
                editor.insert("\t");
            }

            // going to the next line
            Key::Enter if !mods.action_key() && self.mode == EditMode::Insert => {
                editor.cursor_to_newline();
            }

            // deleting a character
            Key::Backspace if !mods.action_key() && self.mode == EditMode::Insert => {
                editor.delete_backwards();
            }
            Key::Backspace if !mods.action_key() && self.mode == EditMode::Normal => {
                editor.move_cursor(Direction::Left);
            }

            // moving in non-normal modes
            Key::ArrowLeft => editor.move_cursor(Direction::Left),
            Key::ArrowRight => editor.move_cursor(Direction::Right),
            Key::ArrowUp => editor.move_cursor(Direction::Up),
            Key::ArrowDown => editor.move_cursor(Direction::Down),

            // classic directory open
            Key::Character(c) if c == "O" && mods.ctrl() && mods.shift() => ctx.submit_command(
                SHOW_OPEN_PANEL.with(
                    FileDialogOptions::new()
                        .select_directories()
                        .accept_command(OPEN_DIRECTORY),
                ),
            ),

            // classic file open
            Key::Character(c) if c == "o" && mods.ctrl() => {
                ctx.submit_command(SHOW_OPEN_PANEL.with(FileDialogOptions::new()));
            }

            // classic file save as
            Key::Character(c) if c == "S" && mods.ctrl() && mods.shift() => {
                ctx.submit_command(SHOW_SAVE_PANEL.with(FileDialogOptions::new()));
            }

            // classic file save
            Key::Character(c) if c == "s" && mods.ctrl() => {
                if editor.file.is_some() {
                    editor.save_to_file();
                } else {
                    ctx.submit_command(SHOW_SAVE_PANEL.with(FileDialogOptions::new()));
                }
            }

            _ => (),
        }
    }
}

/// The different edit modes.
#[derive(Clone, Data, PartialEq)]
pub enum EditMode {
    /// Normal mode.
    Normal,
    /// Insert mode.
    Insert,
    /// Visual mode.
    Visual,
    /// Visual line mode.
    VisualLine,
}

impl Default for EditMode {
    fn default() -> Self {
        EditMode::Normal
    }
}

impl ToString for EditMode {
    fn to_string(&self) -> String {
        match self {
            EditMode::Normal => "NORMAL".to_owned(),
            EditMode::Insert => "INSERT".to_owned(),
            EditMode::Visual => "VISUAL".to_owned(),
            EditMode::VisualLine => "VISUAL LINE".to_owned(),
        }
    }
}
