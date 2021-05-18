//! The activity bar. it is the left bar used to open the file exporer,
//! or the notification panel.

use app_dirs2::{app_root, AppDataType};
use druid::{
    widget::{Flex, SizedBox, Svg, ViewSwitcher},
    Data, Widget, WidgetExt,
};

use crate::{
    app::AppState,
    editor::EDITOR_ID,
    file_explorer::{file_explorer_builder, OPEN_FILE_IN_EDITOR},
    notification_center::notification_center_builder,
    settings::{APP_INFO, BELL_ICON, FILE_ICON, SETTINGS_ICON},
    utils::ButtonController,
};

/// Builds the activity bar.
pub fn activity_bar_builder() -> impl Widget<AppState> {
    let make_activity_button = |svg, tab| {
        Svg::new(svg)
            .fix_width(20.)
            .on_click(move |ctx, data: &mut ActivityBarState, _| {
                if tab == ActivityBarTab::Settings {
                    ctx.submit_command(
                        OPEN_FILE_IN_EDITOR
                            .with(
                                app_root(AppDataType::UserData, &APP_INFO)
                                    .unwrap()
                                    .join("settings.toml"),
                            )
                            .to(EDITOR_ID),
                    )
                } else if data.current_tab == tab {
                    data.current_tab = ActivityBarTab::None
                } else {
                    data.current_tab = tab;
                }
            })
            .controller(ButtonController)
            .padding(15.)
    };

    Flex::column()
        .with_child(make_activity_button(
            FILE_ICON.clone(),
            ActivityBarTab::FileExplorer,
        ))
        .with_child(make_activity_button(
            BELL_ICON.clone(),
            ActivityBarTab::NotificationCenter,
        ))
        .with_flex_spacer(1.)
        .with_child(make_activity_button(
            SETTINGS_ICON.clone(),
            ActivityBarTab::Settings,
        ))
        .expand_height()
        .lens(AppState::activity_bar)
}

/// Builds the view switcher at the right of our activity bar.
pub fn activity_chooser_builder() -> impl Widget<AppState> {
    ViewSwitcher::new(
        |data: &AppState, _| data.activity_bar.current_tab,
        |selector, _, _| match selector {
            ActivityBarTab::None | ActivityBarTab::Settings => Box::new(SizedBox::empty()),
            ActivityBarTab::FileExplorer => Box::new(file_explorer_builder()),
            ActivityBarTab::NotificationCenter => Box::new(notification_center_builder()),
        },
    )
}

/// The state of the activity bar.
#[derive(Clone, Data, Default)]
pub struct ActivityBarState {
    /// The current tab.
    current_tab: ActivityBarTab,
}

/// The different tabs of the activity bar.
/// Our setup is a little bit too complex to be used with a Tabs widget.
#[derive(PartialEq, Clone, Copy, Data)]
pub enum ActivityBarTab {
    /// No tab is open.
    None,
    /// The file explorer is open.
    FileExplorer,
    /// The notification center is open.
    NotificationCenter,
    /// The settings are open.
    Settings,
}

impl Default for ActivityBarTab {
    fn default() -> Self {
        ActivityBarTab::None
    }
}
