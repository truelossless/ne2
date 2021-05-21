//! The embedded terminal emulator in Ne2.

use std::{
    io::{Read, Write},
    mem, thread,
};

use druid::{
    im::Vector,
    keyboard_types::Key,
    text::{RichText, RichTextBuilder},
    widget::{Axis, Controller, Flex, Label, List, Scroll},
    Color, Data, Event, FontStyle, FontWeight, Lens, Selector, Widget, WidgetExt, WidgetId,
};
use portable_pty::{CommandBuilder, NativePtySystem, PtySize, PtySystem, SlavePty};
use vte::{Params, Parser, Perform};

use crate::{
    app::{AppState, SINK},
    utils::{ActionKey, LineEnding},
};

/// The id of the terminal.
const TERMINAL_ID: WidgetId = WidgetId::reserved(2);

/// The Command sent when a new line of the terminal is available.
const NEW_TERM_DATA: Selector<String> = Selector::new("ne2.terminal.new_term_line");

/// The notification sent when the terminal scroll needs to be updated.
const SCROLL_BOTTOM: Selector<()> = Selector::new("ne2.terminal.scroll_bottom");

/// The maximum number of lines to be displayed in the editor.
const MAX_TERM_LINES: usize = 200;

/// Tbe state of the terminal.
#[derive(Data, Clone, Lens, Default)]
pub struct TerminalState {
    /// The current command being typed.
    pub command: String,
    /// The terminal output.
    pub output: Vector<RichText>,
    /// The last line of the output. We keep track of
    /// this to easily append the command being typed client-side.
    pub last_line: String,
    /// Checks if the last line is complete (e.g it is LF-terminated).
    pub last_line_complete: bool,
}

/// A shell in a pty.
#[derive(Clone)]
pub struct Shell {
    /// The command and args used to spawn the shell.
    command: &'static str,
    /// The line ending used to submit a command.
    line_ending: LineEnding,
}

/// The scroll controller. This allows us to scroll to the bottom of
/// the terminal when we receive new data from the shell.
pub struct ScrollController;

impl<T: Data, W: Widget<T>> Controller<T, Scroll<T, W>> for ScrollController {
    fn event(
        &mut self,
        child: &mut Scroll<T, W>,
        ctx: &mut druid::EventCtx,
        event: &Event,
        data: &mut T,
        env: &druid::Env,
    ) {
        child.event(ctx, event, data, env);
        match event {
            Event::Command(cmd) if cmd.is(SCROLL_BOTTOM) => {
                child.scroll_to_on_axis(Axis::Vertical, child.child_size().height);
                ctx.set_handled();
            }

            _ => (),
        }
    }
}

/// The terminal controller.
pub struct TerminalController {
    /// The stdin sender.
    sender: Box<dyn Write + Send>,
    /// The slave process. Windows is not happy when we drop this,
    /// but this works fine without it on Linux.
    _slave: Box<dyn SlavePty>,
    /// The ANSI escape parser.
    parser: AnsiParser,
}

impl TerminalController {
    /// Creates the terminal controller and spawns a shell.
    pub fn new() -> Self {
        let pty_system = NativePtySystem::default();
        let pair = pty_system
            .openpty(PtySize {
                rows: 24,
                cols: 200,
                pixel_width: 0,
                pixel_height: 0,
            })
            .unwrap();

        let cmd = CommandBuilder::new(if cfg!(windows) { "cmd" } else { "bash" });
        pair.slave.spawn_command(cmd).unwrap();

        let mut pty_out = pair.master.try_clone_reader().unwrap();
        let pty_in = pair.master.try_clone_writer().unwrap();

        // read from the pty
        thread::spawn(move || {
            let mut buf = [0; 4096];
            loop {
                if let Some(sink) = SINK.get() {
                    match pty_out.read(&mut buf) {
                        Ok(0) | Err(_) => break,
                        Ok(len) => {
                            sink.submit_command(
                                NEW_TERM_DATA,
                                String::from_utf8_lossy(&buf[..len]).to_string(),
                                TERMINAL_ID,
                            )
                            .unwrap();
                        }
                    }
                }
            }
        });

        Self {
            sender: pty_in,
            _slave: pair.slave,
            parser: AnsiParser::new(),
        }
    }

    /// Updates the last line of the terminal to include the command being typed.
    pub fn update_last_line(&mut self, data: &mut TerminalState) {
        data.output.pop_back();
        data.output.push_back(
            self.parser
                .ansi_escape(&format!("{}{}", data.last_line, data.command)),
        );
    }
}

impl<W: Widget<TerminalState>> Controller<TerminalState, W> for TerminalController {
    fn event(
        &mut self,
        child: &mut W,
        ctx: &mut druid::EventCtx,
        event: &druid::Event,
        data: &mut TerminalState,
        env: &druid::Env,
    ) {
        match event {
            Event::KeyDown(key_event) => {
                match &key_event.key {
                    Key::Enter => {
                        // bash uses lf or cr line endings, cmd and powershell cr or crlf.
                        // cr is the lowest common denominator.
                        data.command.push('\r');

                        let command = mem::take(&mut data.command);
                        self.update_last_line(data);

                        self.sender.write_all(command.as_bytes()).unwrap();
                        self.sender.flush().unwrap();
                    }

                    Key::Backspace => {
                        data.command.pop();
                        self.update_last_line(data);
                    }

                    Key::Character(c) if !key_event.mods.action_key() => {
                        data.command.push_str(&c);
                        self.update_last_line(data);
                    }

                    // CTRL + C (SIGQUIT)
                    Key::Character(c) if c == "c" && key_event.mods.ctrl() => {
                        self.sender.write_all(&[3]).unwrap();
                        self.sender.flush().unwrap();
                    }

                    _ => (),
                }
                ctx.set_handled();
                ctx.submit_command(SCROLL_BOTTOM.to(TERMINAL_ID));
            }

            Event::MouseDown(_) => {
                ctx.request_focus();
            }

            Event::Command(cmd) => {
                if let Some(chunk) = cmd.get(NEW_TERM_DATA) {
                    let mut new_lines = Vec::new();

                    let mut line_iter = chunk.lines();

                    // the first new line can be a continuation of the last line.
                    if !data.last_line_complete {
                        data.output.pop_back();
                        let line = format!("{}{}", data.last_line, line_iter.next().unwrap());
                        new_lines.push(line);
                    }
                    new_lines.extend(line_iter.map(ToOwned::to_owned));

                    let new_lines_len = new_lines.len();
                    let mut new_line_iter = new_lines.into_iter();
                    for line in new_line_iter.by_ref().take(new_lines_len - 1) {
                        data.output
                            .push_back(self.parser.ansi_escape(&format!("{}\n", line)));
                    }

                    let last_line = new_line_iter.next().unwrap();
                    if chunk.ends_with('\n') {
                        data.last_line_complete = true;
                        data.output
                            .push_back(self.parser.ansi_escape(&format!("{}\n", last_line)));
                    } else {
                        data.last_line_complete = false;
                        data.output.push_back(self.parser.ansi_escape(&last_line));
                    }

                    // only display the last X lines
                    let lines_len = data.output.len();
                    data.output = data
                        .output
                        .slice(lines_len.saturating_sub(MAX_TERM_LINES)..);

                    data.last_line = last_line;
                    self.update_last_line(data);
                    ctx.submit_command(SCROLL_BOTTOM.to(TERMINAL_ID));
                    ctx.set_handled();
                }
            }
            _ => (),
        }
        child.event(ctx, event, data, env);
    }
}

// Common terminal colors, using the Windows 10 console theme.
/// Black.
const BLACK: Color = Color::rgb8(12, 12, 12);
/// Red.
const RED: Color = Color::rgb8(197, 15, 31);
/// Green.
const GREEN: Color = Color::rgb8(19, 161, 14);
/// Yellow.
const YELLOW: Color = Color::rgb8(193, 156, 0);
/// Blue.
const BLUE: Color = Color::rgb8(0, 55, 218);
/// Magenta.
const MAGENTA: Color = Color::rgb8(136, 23, 152);
/// Cyan.
const CYAN: Color = Color::rgb8(58, 150, 221);
/// White.
const WHITE: Color = Color::rgb8(204, 204, 204);
/// Bright black.
const BRIGHT_BLACK: Color = Color::rgb8(118, 118, 118);
/// Bright red.
const BRIGHT_RED: Color = Color::rgb8(231, 72, 86);
/// Bright green.
const BRIGHT_GREEN: Color = Color::rgb8(22, 198, 12);
/// Bright yellow.
const BRIGHT_YELLOW: Color = Color::rgb8(249, 241, 165);
/// Bright blue.
const BRIGHT_BLUE: Color = Color::rgb8(59, 120, 255);
/// Bright magenta.
const BRIGHT_MAGENTA: Color = Color::rgb8(180, 0, 158);
/// Bright cyan.
const BRIGHT_CYAN: Color = Color::rgb8(97, 214, 214);
/// Bright white.
const BRIGHT_WHITE: Color = Color::rgb8(242, 242, 242);
/// A parser for terminal sequences that needs to be escaped.
pub struct AnsiParser {
    /// The current index of the parser.
    idx: usize,
    /// The output text builder.
    output: RichTextBuilder,
    /// The current color of the text.
    color: Color,
    /// If the text is underlined.
    underline: bool,
    /// The text's weight.
    weight: FontWeight,
    /// The text's style.
    style: FontStyle,
    /// The vte parser.
    parser: Option<Parser>,
}

impl AnsiParser {
    /// Creates a new AnsiParser.
    pub fn new() -> Self {
        Self {
            idx: 0,
            color: Color::WHITE,
            underline: false,
            weight: FontWeight::NORMAL,
            style: FontStyle::Regular,
            output: RichTextBuilder::new(),
            parser: Some(Parser::new()),
        }
    }

    /// Parses the given text and produces a RichTextBuilder.
    pub fn ansi_escape(&mut self, text: &str) -> RichText {
        // We cannot borrow the parser and call advance() at the same time
        // although it is perfectly safe because advance() doesn't touch the parser.
        // As a workaround, we can take the Parser every time.
        let mut parser = self.parser.take().unwrap();

        for byte in text.as_bytes().iter() {
            parser.advance(self, *byte);
            self.idx += 1;
        }

        self.parser = Some(parser);
        mem::take(&mut self.output).build()
    }
}

impl Perform for AnsiParser {
    fn print(&mut self, c: char) {
        // dbg!(c, self.color.clone());
        self.output
            .push(&String::from(c))
            .style(self.style)
            .text_color(self.color.clone())
            .weight(self.weight)
            .underline(self.underline);
    }

    fn execute(&mut self, _byte: u8) {
        // println!("[execute] {:02x}", byte);
    }

    fn hook(&mut self, _params: &Params, _intermediates: &[u8], _ignore: bool, _c: char) {
        // println!(
        //     "[hook] params={:?}, intermediates={:?}, ignore={:?}, char={:?}",
        //     params, intermediates, ignore, c
        // );
    }

    fn put(&mut self, _byte: u8) {
        // println!("[put] {:02x}", byte);
    }

    fn unhook(&mut self) {
        // println!("[unhook]");
    }

    fn osc_dispatch(&mut self, _params: &[&[u8]], _bell_terminated: bool) {
        // println!(
        //     "[osc_dispatch] params={:?} bell_terminated={}",
        //     params, bell_terminated
        // );
    }

    fn csi_dispatch(&mut self, params: &Params, _intermediates: &[u8], _ignore: bool, _c: char) {
        for param in params.iter() {
            // https://en.wikipedia.org/wiki/ANSI_escape_code
            match param[0] {
                0 => {
                    self.underline = false;
                    self.color = WHITE;
                    self.weight = FontWeight::NORMAL;
                    self.style = FontStyle::Regular;
                }
                1 => self.weight = FontWeight::BOLD,
                2 => self.weight = FontWeight::LIGHT,
                3 => self.style = FontStyle::Italic,
                4 => self.underline = true,
                22 => self.weight = FontWeight::NORMAL,
                23 => self.style = FontStyle::Regular,
                24 => self.underline = false,
                30 => self.color = BLACK,
                31 => self.color = RED,
                32 => self.color = GREEN,
                33 => self.color = YELLOW,
                34 => self.color = BLUE,
                35 => self.color = MAGENTA,
                36 => self.color = CYAN,
                37 => self.color = WHITE,
                39 => self.color = WHITE,
                90 => self.color = BRIGHT_BLACK,
                91 => self.color = BRIGHT_RED,
                92 => self.color = BRIGHT_GREEN,
                93 => self.color = BRIGHT_YELLOW,
                94 => self.color = BRIGHT_BLUE,
                95 => self.color = BRIGHT_MAGENTA,
                96 => self.color = BRIGHT_CYAN,
                97 => self.color = BRIGHT_WHITE,
                _ => (), // println!("[unhandled csi_dispatch] params={:#?}, intermediates={:?}, ignore={:?}, char={:?}", params, intermediates, ignore, c),
            }
        }
    }

    fn esc_dispatch(&mut self, _intermediates: &[u8], _ignore: bool, _byte: u8) {
        // println!(
        //     "[esc_dispatch] intermediates={:?}, ignore={:?}, byte={:02x}",
        //     intermediates, ignore, byte
        // );
    }
}

/// Builds the terminal UI component.
pub fn terminal_builder() -> impl Widget<AppState> {
    Flex::column()
        .with_child(Label::new("cmd"))
        .with_flex_child(
            List::new(|| Label::raw().expand_width())
                .lens(TerminalState::output)
                .scroll()
                .vertical()
                .controller(ScrollController),
            1.,
        )
        .with_spacer(10.)
        .padding(3.)
        .controller(TerminalController::new())
        .with_id(TERMINAL_ID)
        .lens(AppState::terminal)
}
