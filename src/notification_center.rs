//! The notification center. It is used to display various health-related messages,
//! such as the LSP server status, the file highlighting status.
//! It also displays notifications and allows interactivity via buttons.

use core::fmt;
use std::sync::Arc;

use crate::{
    app::AppState,
    lsp::LspServerState,
    settings::{CLOSE_ICON, THEME},
    utils::{ButtonController, ToColor},
};
use druid::{
    im::Vector,
    lens,
    widget::{
        Container, CrossAxisAlignment, Either, Flex, Label, LineBreaking, List, Maybe, ProgressBar,
        SizedBox, Svg,
    },
    ArcStr, Data, Lens, LensExt, Selector, Widget, WidgetExt,
};

/// A command to open a new text notification.
/// Params:
/// 1. The string id of the notification if needed
/// 2. The text of the notification
pub const ADD_TEXT_NOTIFICATION: Selector<(Option<&'static str>, String)> =
    Selector::new("ne2.notification_center.add_text_notification");

/// A command to open a new button notification.
/// Params:
/// 1. The string id of the notification if needed
/// 2. The text of the notification
/// 3. the button text of the notification
/// 4. the callback of the notification
pub const ADD_BUTTON_NOTIFICATION: Selector<(Option<&'static str>, String, String, Callback)> =
    Selector::new("ne2.notification_center.add_button_notification");

/// Updates the progressbar of the given notification.
pub const UPDATE_PROGRESS_BAR: Selector<(&'static str, f64)> =
    Selector::new("ne2.notification_center.update_progress_bar");

/// Hides the button of the given notification.
pub const UPDATE_TEXT: Selector<(&'static str, ArcStr)> =
    Selector::new("ne2.notification_center.update_text");

/// A callback exposing the app state.
pub type Callback = Arc<dyn Fn(&mut (AppState, NotificationState)) + Send + Sync>;

/// The notification center state.
#[derive(Clone, Data, Default, Lens)]
pub struct NotificationCenterState {
    /// All the notifications displayed.
    notifications: Vector<NotificationState>,
    /// The id of the last notification added.
    id: usize,
}

impl NotificationCenterState {
    /// Adds a simple notification to the notification center. Returns the id of the notification added.
    pub fn add_text_notification(&mut self, text: &str) -> NotificationId {
        self.id = self.id.overflowing_add(1).0;
        self.notifications.push_back(NotificationState {
            id: self.id.into(),
            text: Arc::from(text),
            button: None,
            progress_bar: None,
        });

        self.id.into()
    }

    /// Adds a simple notification with the provided id.
    pub fn add_text_notification_with_id(&mut self, id: Option<&'static str>, text: &str) {
        if id.is_none() {
            self.id = self.id.overflowing_add(1).0;
        }

        let id = id.map(|s| s.into()).unwrap_or_else(|| self.id.into());
        self.notifications.push_back(NotificationState {
            id,
            text: Arc::from(text),
            button: None,
            progress_bar: None,
        });
    }

    /// Adds a notification with a clickable button. Returns the id of the notification added.
    pub fn add_button_notification(
        &mut self,
        text: &str,
        button_text: &str,
        callback: Callback,
    ) -> NotificationId {
        self.id = self.id.overflowing_add(1).0;
        self.notifications.push_back(NotificationState {
            id: self.id.into(),
            text: Arc::from(text),
            button: Some(NotificationButtonState {
                text: Arc::from(button_text),
                callback,
            }),
            progress_bar: None,
        });

        self.id.into()
    }

    /// Adds a notification with a clickable button with the provided id.
    pub fn add_button_notification_with_id(
        &mut self,
        id: Option<&'static str>,
        text: &str,
        button_text: &str,
        callback: Callback,
    ) {
        if id.is_none() {
            self.id = self.id.overflowing_add(1).0;
        }

        let id = id.map(|s| s.into()).unwrap_or_else(|| self.id.into());
        self.notifications.push_back(NotificationState {
            id,
            text: Arc::from(text),
            button: Some(NotificationButtonState {
                text: Arc::from(button_text),
                callback,
            }),
            progress_bar: None,
        });
    }

    /// Gets the notification with the corresponding id, if it hasn't been closed.
    pub fn get_notification(&mut self, id: &NotificationId) -> Option<&mut NotificationState> {
        self.notifications.iter_mut().find(|notif| notif.id == *id)
    }

    /// Closes the notification with the corresponding id.
    pub fn close_notification(&mut self, id: &NotificationId) {
        self.notifications.retain(|notif| notif.id != *id)
    }

    /// Clears all notification.
    pub fn clear(&mut self) {
        self.notifications.clear()
    }
}

/// A notification.
#[derive(Clone, Data, Lens, Debug)]
pub struct NotificationState {
    /// THe id of the notification.
    /// This can be either a number (auto incremented) or a string.
    /// Having a string as a key is useful when the notification has to be
    /// reachable from another thread.
    pub id: NotificationId,
    /// The text of the notification.
    pub text: ArcStr,
    /// If the notification features a button.
    pub button: Option<NotificationButtonState>,
    /// If the notification features a progressbar.
    pub progress_bar: Option<f64>,
}

/// A notification with a button.
#[derive(Clone, Data, Lens)]
pub struct NotificationButtonState {
    /// The text on the button.
    text: ArcStr,
    /// The callback to the function to execute on a click.
    callback: Callback,
}

impl fmt::Debug for NotificationButtonState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "button_text: {}", self.text)
    }
}

/// The id of a notification.
#[derive(Clone, PartialEq, Data, Debug)]
pub enum NotificationId {
    /// A unique number representing this id.
    Number(usize),
    /// An unique string representing this id.
    String(&'static str),
}

impl From<&'static str> for NotificationId {
    fn from(string: &'static str) -> Self {
        NotificationId::String(string)
    }
}

impl From<usize> for NotificationId {
    fn from(number: usize) -> Self {
        NotificationId::Number(number)
    }
}

/// Builds a card-like element to display notifications in it.
fn card<T: Data>(widget: impl Widget<T> + 'static) -> impl Widget<T> {
    Container::new(widget.padding(5.).expand_width())
        .background(THEME.settings.background.unwrap().to_color())
        .rounded(2.)
}

/// Builds a notification with a possible click action.
pub fn notification() -> impl Widget<(AppState, NotificationState)> {
    let notif_lens = lens::Identity.map(
        |d: &(AppState, NotificationState)| d.1.clone(),
        |d: &mut (AppState, NotificationState), new_d| d.1 = new_d,
    );

    card(
        Flex::column()
            .cross_axis_alignment(CrossAxisAlignment::End)
            .with_child(
                Svg::new(CLOSE_ICON.clone())
                    .fix_width(10.)
                    .controller(ButtonController)
                    .on_click(
                        |_, (shared, notif): &mut (AppState, NotificationState), _| {
                            shared.notification_center.close_notification(&notif.id)
                        },
                    ),
            )
            .with_child(
                Label::raw()
                    .with_line_break_mode(LineBreaking::WordWrap)
                    .lens(notif_lens.clone().then(NotificationState::text))
                    .expand_width(),
            )
            // we can't use a Maybe because the lens will prevent us from accessing the AppState
            // in the callback. As a result, This gets pretty messy.
            .with_child(Either::new(
                |(_, notif): &(AppState, NotificationState), _| notif.button.is_some(),
                Label::dynamic(|(_, notif): &(AppState, NotificationState), _| {
                    notif
                        .button
                        .as_ref()
                        .map(|button| button.text.to_string())
                        .unwrap_or_default()
                })
                .padding(3.)
                .border(THEME.settings.foreground.unwrap().to_color(), 1.)
                .padding((0., 10., 0., 0.))
                .controller(ButtonController)
                .on_click(
                    move |_, data: &mut (AppState, NotificationState), _| {
                        let callback = data.1.button.as_ref().unwrap().callback.clone();
                        callback(data);
                    },
                ),
                SizedBox::empty(),
            ))
            .with_spacer(15.)
            .with_child(
                Maybe::or_empty(ProgressBar::new)
                    .lens(notif_lens.then(NotificationState::progress_bar))
                    .expand_width(),
            ),
    )
}

/// Builds the notification center.
pub fn notification_center_builder() -> impl Widget<AppState> {
    // if we lens our list of notifications like we may do usually, we
    // will not have access to the app state for the notification callbacks.
    // with this lens, we can have access to both the shared and local state.
    // https://github.com/derekdreery/druid/blob/selectable_list/druid/examples/list.rs
    let notifs_lens = lens::Identity.map(
        |d: &AppState| (d.clone(), d.notification_center.notifications.clone()),
        |d: &mut AppState, (new_d, _): (AppState, Vector<NotificationState>)| *d = new_d,
    );

    Flex::column()
        .with_child(
            List::new(|| {
                card(
                    Label::dynamic(|data: &LspServerState, _| {
                        let lsp_server = data.server.lock().unwrap();
                        format!("LSP server: {}\nstatus: {}", lsp_server.name, data.status)
                    })
                    .with_line_break_mode(LineBreaking::WordWrap),
                )
            })
            .lens(AppState::lsp_servers),
        )
        .with_spacer(10.)
        .with_child(List::new(notification).with_spacing(10.).lens(notifs_lens))
        .fix_width(300.)
        .padding((0., 20., 20., 20.))
        .scroll()
        .vertical()
}
