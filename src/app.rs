//! The main app taking care of unifying all the UI elements.

use std::{
    env, mem,
    path::{Path, PathBuf},
    sync::Arc,
};

use crate::{
    activity_bar::{activity_bar_builder, activity_chooser_builder, ActivityBarState},
    editor::{editor_builder, EditorState, OPEN_DIRECTORY, SAVE_FILE_AS_AND_EXIT},
    file_explorer::FileNode,
    notification_center::{
        NotificationCenterState, NotificationId, ADD_BUTTON_NOTIFICATION, ADD_TEXT_NOTIFICATION,
        UPDATE_PROGRESS_BAR, UPDATE_TEXT,
    },
    status_bar::status_bar_builder,
    terminal::{terminal_builder, TerminalState},
    vim::VimState,
};
use druid::{
    commands::{OPEN_FILE, SAVE_FILE_AS},
    widget::{CrossAxisAlignment, Flex, Split},
    AppDelegate, Application, Data, ExtEventSink, Handled, Lens, Widget, WidgetExt,
};
use once_cell::sync::OnceCell;

/// The event sink of the application.
/// It is used by a lot of different states so it makes sense
/// to register it as a global variable and define it at the
/// start of the application.
pub static SINK: OnceCell<ExtEventSink> = OnceCell::new();

/// The whole application state
#[derive(Clone, Data, Lens)]
pub struct AppState {
    /// The activity bar state.
    pub activity_bar: ActivityBarState,
    /// The editor state.
    pub editor: EditorState,
    /// The notification center state.
    pub notification_center: NotificationCenterState,
    /// The project's root directory.
    pub project: FileNode,
    /// The terminal state.
    pub terminal: TerminalState,
    /// The vim state.
    pub vim: VimState,
}

impl AppState {
    /// Builds the default app state.
    /// If ne2 ia open from a file/folder, the first parameter is filled.
    pub fn new(start_path: Option<PathBuf>) -> Self {
        // get the project root
        let project_path = start_path
            .as_ref()
            .map(|path| {
                if path.is_file() {
                    path.parent().unwrap().to_owned()
                } else {
                    path.to_owned()
                }
            })
            .unwrap_or_else(|| env::current_exe().unwrap().parent().unwrap().to_owned());

        let file_path = start_path.filter(|path| path.is_file());

        Self {
            project: FileNode::new_loaded_dir(&project_path),
            activity_bar: ActivityBarState::default(),
            editor: EditorState::new(file_path),
            notification_center: NotificationCenterState::default(),
            terminal: TerminalState::default(),
            vim: VimState::default(),
        }
    }

    /// Opens a file in the editor.
    pub fn open_file(&mut self, path: &Path) {
        self.notification_center.clear();
        let old_editor = mem::replace(&mut self.editor, EditorState::new(Some(path.to_owned())));
        self.editor.lines_displayed = old_editor.lines_displayed;
        self.editor.update_language();
    }
}

/// Builds the ne2 UI
pub fn ui_builder() -> impl Widget<AppState> {
    let editor_term_split = Split::rows(editor_builder().expand(), terminal_builder())
        .draggable(true)
        .bar_size(0.)
        .split_point(0.7);

    Flex::column()
        .with_flex_child(
            Flex::row()
                .cross_axis_alignment(CrossAxisAlignment::Start)
                .with_child(activity_bar_builder())
                .with_child(activity_chooser_builder())
                .with_flex_child(editor_term_split, 1.),
            1.,
        )
        .with_child(status_bar_builder())
}

/// The app delegate.
pub struct Delegate;
impl AppDelegate<AppState> for Delegate {
    fn window_added(
        &mut self,
        _id: druid::WindowId,
        _data: &mut AppState,
        _env: &druid::Env,
        ctx: &mut druid::DelegateCtx,
    ) {
        let _ = SINK.set(ctx.get_external_handle());
    }

    fn command(
        &mut self,
        _ctx: &mut druid::DelegateCtx,
        _target: druid::Target,
        cmd: &druid::Command,
        data: &mut AppState,
        _env: &druid::Env,
    ) -> druid::Handled {
        // we handle here the results of the SHOW_* commands.
        // the editor can be deep down the widget tree, and using Target::Auto
        // is inefficient.
        if let Some(path) = cmd.get(OPEN_FILE) {
            data.open_file(path.path());
            return Handled::Yes;
        }

        if let Some(path) = cmd.get(OPEN_DIRECTORY) {
            data.notification_center.clear();
            data.project = FileNode::new_loaded_dir(path.path());
            return Handled::Yes;
        }

        if let Some(path) = cmd.get(SAVE_FILE_AS) {
            data.editor.file = Some(Arc::from(path.path()));
            data.editor.save_to_file();
            data.editor.update_language();
            return Handled::Yes;
        }

        if let Some(path) = cmd.get(SAVE_FILE_AS_AND_EXIT) {
            data.editor.file = Some(Arc::from(path.path()));
            data.editor.save_to_file();
            Application::global().quit();
            return Handled::Yes;
        }

        // here we handle the commands of the notification center.
        // since it's not always open, a controller would miss certain notifications.
        if let Some((id, text)) = cmd.get(ADD_TEXT_NOTIFICATION) {
            data.notification_center
                .add_text_notification_with_id(*id, text);
            return Handled::Yes;
        }

        if let Some((id, text, button_text, callback)) = cmd.get(ADD_BUTTON_NOTIFICATION) {
            data.notification_center.add_button_notification_with_id(
                *id,
                text,
                button_text,
                callback.clone(),
            );
            return Handled::Yes;
        }

        if let Some((id, progress)) = cmd.get(UPDATE_PROGRESS_BAR) {
            if let Some(notif) = data
                .notification_center
                .get_notification(&NotificationId::String(id))
            {
                notif.progress_bar = Some(*progress);
            }
            return Handled::Yes;
        }

        if let Some((id, text)) = cmd.get(UPDATE_TEXT) {
            if let Some(notif) = data
                .notification_center
                .get_notification(&NotificationId::String(id))
            {
                notif.text = text.clone();
            }
            return Handled::Yes;
        }

        Handled::No
    }
}
