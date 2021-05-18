//! The file explorer.

use std::{
    fs,
    path::{Path, PathBuf},
    sync::Arc,
};

use druid::{
    im::Vector,
    widget::{CrossAxisAlignment, Either, Flex, Label, List, Maybe, SizedBox, Svg},
    ArcStr, Data, Lens, Selector, Widget, WidgetExt,
};

use crate::{
    app::AppState,
    editor::EDITOR_ID,
    settings::{ARROW_DOWN, ARROW_RIGHT},
    utils::{ButtonController, HighlightController},
};

/// A command to open a file in the editor.
pub const OPEN_FILE_IN_EDITOR: Selector<PathBuf> =
    Selector::new("ne2.file_explorer.open_file_in_editor");

/// A node of our file tree.
#[derive(Clone, Data, Lens)]
pub struct FileNode {
    /// The name of the file /  directory.
    name: ArcStr,
    /// The path to the file / directory.
    path: Arc<Path>,
    /// The contents of the directory if it's one.
    dir: Option<DirContents>,
}

#[derive(Default, Clone, Data, Lens)]
/// The contents of a directory.
struct DirContents {
    /// The files / directories inside this directory.
    files: Vector<FileNode>,
    /// Whether or not this directory has been loaded.
    is_loaded: bool,
    /// Whether or not this directory is visible on screen.
    is_visible: bool,
}

impl FileNode {
    /// Creates a new directory node.
    fn new_dir(path: &Path) -> Self {
        Self {
            name: path.file_name().unwrap().to_string_lossy().into(),
            path: path.into(),
            dir: Some(DirContents::default()),
        }
    }

    /// Creates a new directory node and loads directly its content.
    #[inline]
    pub fn new_loaded_dir(path: &Path) -> Self {
        let mut dir = FileNode::new_dir(path);
        dir.load();
        dir
    }

    /// Creates a new file leaf.
    fn new_file(path: &Path) -> Self {
        Self {
            name: path.file_name().unwrap().to_string_lossy().into(),
            path: path.into(),
            dir: None,
        }
    }

    /// Loads the children of the directory node if needed.
    fn load(&mut self) {
        let dir = self.dir.as_mut().unwrap();

        if dir.is_loaded {
            return;
        }

        // we show in our tree a view of the directories, then the files.
        // The results are sorted alphabetically.
        // This is why we need to diffenciate here files and directories.
        // Per the fs::read_dir spec the files and directory are not
        // guaranteed to be sorted, but in practice they are, on Linux
        // and Windows at least.
        let mut files = Vector::new();
        let mut dirs = Vector::new();

        for child in fs::read_dir(&self.path).unwrap() {
            let child = child.unwrap();
            let path = child.path();

            if path.is_dir() {
                dirs.push_back(FileNode::new_dir(&path));
            } else {
                files.push_back(FileNode::new_file(&path));
            }
        }

        dir.files.append(dirs);
        dir.files.append(files);
        dir.is_loaded = true;
    }

    /// Toggles between showing files and hiding files.
    fn toggle(&mut self) {
        self.load();
        let dir = self.dir.as_mut().unwrap();
        dir.is_visible = !dir.is_visible;
    }
}

/// Builds the file explorer.
/// If we don't have any project open, prompt to open a directory.
pub fn file_explorer_builder() -> impl Widget<AppState> {
    let tree_root = || {
        Maybe::new(
            || List::new(tree_builder).lens(DirContents::files),
            || Label::new("ITS REALLY empty"),
        )
        .lens(FileNode::dir)
    };

    Maybe::new(tree_root, || Label::new("It's empty there!"))
        .padding((0., 20., 20., 20.))
        .scroll()
        .lens(AppState::project)
}

/// One level of the file explorer.
/// The tree widget in the druid nursery isn't advanced enough for our use.
/// In particular, it doesn't allow lazy-loading.
fn tree_builder() -> impl Widget<FileNode> {
    Flex::column()
        .cross_axis_alignment(CrossAxisAlignment::Start)
        .with_child(
            Flex::row()
                .with_child(
                    Maybe::or_empty(|| {
                        Either::new(
                            |data: &DirContents, _| data.is_visible,
                            Svg::new(ARROW_RIGHT.clone()),
                            Svg::new(ARROW_DOWN.clone()),
                        )
                    })
                    .lens(FileNode::dir),
                )
                .with_child(
                    Label::raw()
                        .controller(HighlightController)
                        .lens(FileNode::name),
                )
                .controller(ButtonController)
                .on_click(|ctx, data, _| {
                    if data.dir.is_some() {
                        data.toggle();
                    } else {
                        ctx.submit_command(
                            OPEN_FILE_IN_EDITOR
                                .with(data.path.to_path_buf())
                                .to(EDITOR_ID),
                        );
                    }
                }),
        )
        .with_child(
            Maybe::or_empty(|| {
                Either::new(
                    |data, _| data.is_visible,
                    List::new(|| tree_builder().padding((10., 0., 0., 0.)))
                        .lens(DirContents::files),
                    SizedBox::empty(),
                )
            })
            .lens(FileNode::dir),
        )
}
