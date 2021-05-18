//! The status bar component of the UI.

use druid::{
    widget::{Flex, Label},
    Widget, WidgetExt,
};

use crate::{app::AppState, settings::THEME, utils::ToColor};

/// Builds the status bar.
pub fn status_bar_builder() -> impl Widget<AppState> {
    let edit_mode = Label::dynamic(|data: &AppState, _| data.vim.mode.to_string());
    let command = Label::dynamic(|data: &AppState, _| {
        data.vim
            .command
            .as_ref()
            .map(ToString::to_string)
            .unwrap_or_else(String::new)
    });
    let action = Label::dynamic(|data: &AppState, _| data.vim.action.to_string());
    let language = Label::dynamic(|data: &AppState, _| data.editor.language.to_string());
    let line_ending = Label::dynamic(|data: &AppState, _| data.editor.line_ending.to_string());

    Flex::row()
        .with_child(edit_mode)
        .with_child(command)
        .with_flex_spacer(1.)
        .with_child(action)
        .with_child(language)
        .with_child(line_ending)
        .fix_height(30.)
        .background(THEME.settings.background.unwrap().to_color())
}
