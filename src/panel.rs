//! The panel at the bottom of the editor, with
//! the problems view and the terminal view.

use druid::{
    theme::{TEXT_COLOR, UI_FONT},
    widget::{Flex, Label, List, Maybe, Svg, Tabs, TabsTransition, ViewSwitcher},
    Color, Widget, WidgetExt,
};

use crate::{
    app::AppState,
    lsp::{DiagnosticSeverity, DiagnosticState, LspServerState},
    settings::{
        DIAGNOSTIC_ERROR_ICON, DIAGNOSTIC_HINT_ICON, DIAGNOSTIC_WARNING_ICON, FONT, SMALL_FONT,
    },
    terminal::terminal_builder,
};

/// Builds a diagnostic.
fn diagnostic() -> impl Widget<DiagnosticState> {
    Flex::row()
        .with_child(ViewSwitcher::new(
            |data: &DiagnosticState, _| data.severity.clone(),
            |severity: &DiagnosticSeverity, _, _| {
                Box::new(match severity {
                    DiagnosticSeverity::Error => Svg::new(DIAGNOSTIC_ERROR_ICON.clone()),
                    DiagnosticSeverity::Warning => Svg::new(DIAGNOSTIC_WARNING_ICON.clone()),
                    DiagnosticSeverity::Hint => Svg::new(DIAGNOSTIC_HINT_ICON.clone()),
                })
            },
        ))
        .with_spacer(5.)
        .with_child(
            Label::raw()
                .lens(DiagnosticState::message)
                .env_scope(|env, _| env.set(TEXT_COLOR, Color::WHITE)),
        )
        .with_spacer(5.)
        .with_child(
            Maybe::or_empty(Label::raw)
                .lens(DiagnosticState::source)
                .env_scope(|env, _| env.set(UI_FONT, SMALL_FONT.clone())),
        )
        .with_spacer(5.)
        .with_child(
            Maybe::or_empty(Label::raw)
                .lens(DiagnosticState::code)
                .env_scope(|env, _| env.set(UI_FONT, SMALL_FONT.clone()))
                .padding((0., 0., 2., 0.)),
        )
        .with_child(Label::dynamic(|data: &DiagnosticState, _| {
            format!("[{}, {}]", data.line, data.character)
        }))
        .padding(5.)
}

/// Builds the bottom panel.
pub fn panel_builder() -> impl Widget<AppState> {
    Tabs::new()
        .with_tab(
            "PROBLEMS",
            List::new(|| List::new(diagnostic).lens(LspServerState::diagnostics))
                .lens(AppState::lsp_servers)
                .scroll()
                .vertical()
                .expand_width(),
        )
        .with_tab(
            "TERMINAL",
            terminal_builder().env_scope(|env, _| env.set(UI_FONT, FONT.clone())),
        )
        .with_transition(TabsTransition::Instant)
        .env_scope(|env, _| env.set(UI_FONT, SMALL_FONT.clone()))
}
