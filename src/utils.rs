//! Various utilities.

use druid::{
    text::TextStorage,
    widget::{Controller, RawLabel, SvgData},
    Color, Cursor, Data, LifeCycle, Modifiers, Widget,
};

use crate::settings::THEME;

/// A trait to extend druid's Modifiers functionality.
pub trait ActionKey {
    /// Whether or not an action key is pressed (CTRL or ALT).
    fn action_key(&self) -> bool;
}

impl ActionKey for Modifiers {
    #[inline]
    fn action_key(&self) -> bool {
        self.ctrl() || self.alt()
    }
}

#[derive(Clone, Data, PartialEq)]
/// The different line endings.
pub enum LineEnding {
    /// Unix-like line feed endings.
    Lf,
    /// Windows-like carriage return + line feed endings.
    Crlf,
}

impl ToString for LineEnding {
    fn to_string(&self) -> String {
        match self {
            LineEnding::Crlf => "CRLF",
            LineEnding::Lf => "LF",
        }
        .to_owned()
    }
}

impl LineEnding {
    /// Returns the corresponding line ending as characters.
    pub fn to_chars(&self) -> &'static str {
        match self {
            LineEnding::Crlf => "\r\n",
            LineEnding::Lf => "\n",
        }
    }
}

/// Changes the fill color of an SVG with a little hack.
pub fn svg_fill_color(svg: &str, color: Color) -> SvgData {
    let (r, g, b, _) = color.as_rgba8();
    let rgba = format!("rgb({}, {}, {})", r, g, b);

    // we rely on the fact that svg files starts with "<svg"
    format!("<svg fill=\"{}\"{}", rgba, &svg[4..])
        .parse()
        .unwrap()
}

/// A trait to convert a color to a druid color.
pub trait ToColor {
    /// Converts a color to a druid color.
    fn to_color(&self) -> Color;
}

impl ToColor for syntect::highlighting::Color {
    fn to_color(&self) -> Color {
        Color::rgba8(self.r, self.g, self.b, self.a)
    }
}

/// A controller making any widget look like a button.
pub struct ButtonController;

impl<T, W: Widget<T>> Controller<T, W> for ButtonController {
    fn event(
        &mut self,
        child: &mut W,
        ctx: &mut druid::EventCtx,
        event: &druid::Event,
        data: &mut T,
        env: &druid::Env,
    ) {
        ctx.set_cursor(&Cursor::Pointer);
        child.event(ctx, event, data, env);
    }
}

/// A controller which changes the font color of a label on hover.
pub struct HighlightController;

impl<T: Data + TextStorage> Controller<T, RawLabel<T>> for HighlightController {
    fn lifecycle(
        &mut self,
        child: &mut RawLabel<T>,
        ctx: &mut druid::LifeCycleCtx,
        event: &druid::LifeCycle,
        data: &T,
        env: &druid::Env,
    ) {
        // this often crashes with "TextLayout::draw called without rebuilding layout object"
        // when scrolling in debug mode. But this doesn't happen in release so we will keep
        // this like that for now.
        match event {
            LifeCycle::HotChanged(true) => {
                child.set_text_color(THEME.settings.background.unwrap().to_color());
                ctx.request_layout();
            }
            LifeCycle::HotChanged(false) => {
                child.set_text_color(THEME.settings.foreground.unwrap().to_color());
                ctx.request_layout();
            }

            _ => child.lifecycle(ctx, event, data, env),
        }
    }
}
