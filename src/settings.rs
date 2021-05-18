//! Everything related to the settings, theme, and the parsing of settings.toml.

use std::{fs, io::Read};

use app_dirs2::{app_root, AppDataType, AppInfo};
use druid::{widget::SvgData, FontDescriptor, FontFamily, FontStyle, FontWeight};
use fs::File;
use once_cell::{sync::Lazy, unsync};
use serde::Deserialize;
use syntect::{
    highlighting::{Theme, ThemeSet},
    parsing::SyntaxSet,
};

use crate::utils::{svg_fill_color, ToColor};

/// The ne2 directory used to store the settings.toml file, and the lsp servers.
pub const APP_INFO: AppInfo = AppInfo {
    /// Name of the editor.
    name: "ne2",
    /// Name of myself :).
    author: "truelossless",
};

/// The default settings.toml file.
pub const DEFAULT_SETTINGS: &str = include_str!("../res/settings.toml");

/// The syntax set.
pub static PS: Lazy<SyntaxSet> = Lazy::new(SyntaxSet::load_defaults_newlines);

/// The theme set.
static TS: Lazy<ThemeSet> = Lazy::new(ThemeSet::load_defaults);

/// The parsed settings.
static SETTINGS: Lazy<Settings> = Lazy::new(|| {
    let settings_dir = app_root(AppDataType::UserData, &APP_INFO).unwrap();
    let settings_file = settings_dir.join("settings.toml");

    // open the settings.toml file or if it doesn't exist copy the default settings.toml.
    File::open(&settings_file)
        .ok()
        .map(|mut file| {
            let mut buf = String::new();
            file.read_to_string(&mut buf).unwrap();
            toml::from_str::<Settings>(&buf).unwrap_or_else(|_| {
                eprintln!("Errors in settings.toml. Using default config.");
                toml::from_str::<Settings>(DEFAULT_SETTINGS).unwrap()
            })
        })
        .unwrap_or_else(|| {
            fs::write(&settings_file, DEFAULT_SETTINGS).unwrap();
            toml::from_str::<Settings>(DEFAULT_SETTINGS).unwrap()
        })
});

/// The theme.
pub static THEME: Lazy<&'static Theme> = Lazy::new(|| {
    TS.themes
        .get(&SETTINGS.theme.name)
        .unwrap_or_else(|| &TS.themes["InspiredGitHub"])
});

/// The font used in the editor.
pub static FONT: Lazy<FontDescriptor> = Lazy::new(|| SETTINGS.theme.font.clone().into());

/// The bell icon.
pub const BELL_ICON: unsync::Lazy<SvgData> = unsync::Lazy::new(|| {
    svg_fill_color(
        include_str!("../res/icon/bell.svg"),
        THEME.settings.background.unwrap().to_color(),
    )
});

/// The close icon.
pub const CLOSE_ICON: unsync::Lazy<SvgData> = unsync::Lazy::new(|| {
    svg_fill_color(
        include_str!("../res/icon/close.svg"),
        THEME.settings.foreground.unwrap().to_color(),
    )
});

/// The file icon.
pub const FILE_ICON: unsync::Lazy<SvgData> = unsync::Lazy::new(|| {
    svg_fill_color(
        include_str!("../res/icon/file.svg"),
        THEME.settings.background.unwrap().to_color(),
    )
});

/// The settings icon.
pub const SETTINGS_ICON: unsync::Lazy<SvgData> = unsync::Lazy::new(|| {
    svg_fill_color(
        include_str!("../res/icon/settings.svg"),
        THEME.settings.background.unwrap().to_color(),
    )
});

/// The right dropdown arrow.
pub const ARROW_RIGHT: unsync::Lazy<SvgData> = unsync::Lazy::new(|| {
    svg_fill_color(
        include_str!("../res/icon/arrow_right.svg"),
        THEME.settings.background.unwrap().to_color(),
    )
});

/// The down dropdown arrow.
pub const ARROW_DOWN: unsync::Lazy<SvgData> = unsync::Lazy::new(|| {
    svg_fill_color(
        include_str!("../res/icon/arrow_down.svg"),
        THEME.settings.background.unwrap().to_color(),
    )
});

#[derive(Deserialize)]
/// Representation of the Ne2 settings, which is the settings.toml file.
struct Settings {
    /// The theme.
    theme: AppTheme,
}

// #[derive(Deserialize)]
// #[serde(untagged)]
// /// Representation of a Color in Serde.
// pub enum Color {
//     /// A RGB representation.
//     Rgb([u8; 3]),
//     /// A RGBA representation.
//     Rgba([u8; 4]),
// }

// impl ToColor for Color {
//     fn to_color(&self) -> druid::Color {
//         match self {
//             Color::Rgb([r, g, b]) => druid::Color::rgb8(*r, *g, *b),
//             Color::Rgba([r, g, b, a]) => druid::Color::rgba8(*r, *g, *b, *a),
//         }
//     }
// }

/// Representation of a system font in Serde.
#[derive(Deserialize, Clone)]
#[serde(untagged)]
pub enum Font {
    /// The font name as a string.
    FontName(String),
    /// A more detailed font object.
    FullFont {
        /// The font name as a string.
        name: String,
        /// The font size.
        size: Option<f64>,
        /// The font weight.
        weight: Option<String>,
        /// The font style.
        style: Option<String>,
    },
}

impl From<Font> for FontDescriptor {
    fn from(font: Font) -> Self {
        match font {
            Font::FontName(name) => {
                FontDescriptor::new(FontFamily::new_unchecked(name)).with_size(16.)
            }
            Font::FullFont {
                name,
                size,
                weight,
                style,
            } => {
                let weight = weight
                    .map(|weight_str| match weight_str.as_ref() {
                        "thin" => FontWeight::THIN,
                        "hairline" => FontWeight::HAIRLINE,
                        "extralight" => FontWeight::EXTRA_LIGHT,
                        "light" => FontWeight::LIGHT,
                        "regular" => FontWeight::REGULAR,
                        "normal" => FontWeight::NORMAL,
                        "medium" => FontWeight::MEDIUM,
                        "semibold" => FontWeight::SEMI_BOLD,
                        "bold" => FontWeight::BOLD,
                        "extrabold" => FontWeight::BOLD,
                        "black" => FontWeight::BLACK,
                        "heavy" => FontWeight::HEAVY,
                        "extrablack" => FontWeight::EXTRA_BLACK,
                        _ => panic!("{} is not a valid font weight!", weight_str),
                    })
                    .unwrap_or_default();

                let style = style
                    .map(|style_str| match style_str.as_ref() {
                        "regular" => FontStyle::Regular,
                        "italic" => FontStyle::Italic,
                        _ => panic!("{} is not a valid font style !", style_str),
                    })
                    .unwrap_or_default();

                FontDescriptor::new(FontFamily::new_unchecked(name))
                    .with_size(size.unwrap_or(16.))
                    .with_weight(weight)
                    .with_style(style)
            }
        }
    }
}

/// Representation of the editor theme.
#[derive(Deserialize)]
pub struct AppTheme {
    /// The font used in the editor.
    font: Font,
    /// The name of the Sublime Text theme used.
    name: String,
}
