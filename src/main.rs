//! Ne2 (pronounced [neato](https://www.reddit.com/r/tf2/comments/4njl3p/jill_on_facepunch_the_next_update_is_gonna_be/)) is a lightweight IDE with vim-like motions.
//! It is specifically designed to be performant and handles very well large text files.
//! test

#![windows_subsystem = "windows"]
#![deny(clippy::missing_docs_in_private_items)]
// some immutable consts are initialized with once_cell and throw false positives
#![allow(clippy::borrow_interior_mutable_const)]
#![allow(clippy::declare_interior_mutable_const)]

use app::{ui_builder, AppState};
use druid::{AppLauncher, WindowDesc};
use settings::{FONT, THEME};
use std::{env, path::PathBuf};
use utils::ToColor;

use crate::app::Delegate;

mod activity_bar;
mod app;
mod editor;
mod file_explorer;
mod highlight;
mod lsp;
mod notification_center;
mod panel;
mod settings;
mod status_bar;
mod terminal;
mod tooltip;
mod utils;
mod vim;

fn main() {
    let window = WindowDesc::new(ui_builder())
        .title("Ne2")
        .window_size((600., 600.));

    let mut launcher = AppLauncher::with_window(window)
        .configure_env(|env, _| {
            env.set(druid::theme::PROGRESS_BAR_RADIUS, 0.);
            env.set(druid::theme::UI_FONT, FONT.clone());
            env.set(
                druid::theme::TEXT_COLOR,
                THEME.settings.foreground.unwrap().to_color(),
            );
        })
        .delegate(Delegate);

    if cfg!(debug_assertions) {
        launcher = launcher.log_to_console()
    }

    let start_path = env::args().nth(1).map(PathBuf::from);
    launcher.launch(AppState::new(start_path)).unwrap()
}
