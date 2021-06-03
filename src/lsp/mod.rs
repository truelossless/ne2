//! A module handling Language Server Protocol interactions.
//!
//! A good chunk of the code have been adapted from https://github.com/rust-analyzer/lsp-server.
//! I can't really use directly this crate as it's more for servers than for clients.

mod message;

use app_dirs2::{app_dir, AppDataType};
use crossbeam_channel::{unbounded, Receiver, Sender};
use druid::{im::Vector, Data, Lens, Selector, Target};
use lsp_types::{
    notification::{
        DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, DidSaveTextDocument,
        Notification as NotificationTrait,
    },
    request::{Initialize, Request as RequestTrait},
    ClientCapabilities, ClientInfo, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, InitializeParams, Position, Range,
    TextDocumentContentChangeEvent, TextDocumentIdentifier, TextDocumentItem, Url,
    VersionedTextDocumentIdentifier,
};
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use std::{
    collections::HashMap,
    fmt::Display,
    fs::{self, File},
    io::{self, BufRead, BufReader, Write},
    path::{Path, PathBuf},
    process::{self, Child, Command, Stdio},
    sync::{Arc, Mutex},
    thread,
};
use xi_rope::rope::Utf16CodeUnitsMetric;

#[cfg(windows)]
use std::os::windows::process::CommandExt;

use crate::{
    app::{AppState, SINK},
    editor::DeltaState,
    notification_center::{
        Callback, NotificationState, ADD_BUTTON_NOTIFICATION, ADD_TEXT_NOTIFICATION,
        UPDATE_PROGRESS_BAR, UPDATE_TEXT,
    },
    settings::APP_INFO,
};

/// Sent when a new LSP message arrives.
pub const NEW_LSP_MESSAGE: Selector<String> = Selector::new("ne2.lsp.new_lsp_message");

/// A diagnostic provided by the LSP server.
#[derive(Clone, Data, Lens, Debug)]
pub struct DiagnosticState {
    /// The diagnostic message.
    pub message: String,
    /// The severity of the diagnostic.
    pub severity: DiagnosticSeverity,
    /// The source of the diagnostic.
    pub source: Option<String>,
    /// The code of the diagnostic.
    pub code: Option<String>,
    /// The start line of the diagnostic.
    pub line: u32,
    /// The start character of the diagnostic.
    pub character: u32,
}

impl From<lsp_types::Diagnostic> for DiagnosticState {
    fn from(diagnostic: lsp_types::Diagnostic) -> Self {
        Self {
            message: diagnostic.message,
            severity: diagnostic.severity.into(),
            source: diagnostic.source,
            code: diagnostic.code.map(|code| match code {
                lsp_types::NumberOrString::Number(i) => i.to_string(),
                lsp_types::NumberOrString::String(s) => s,
            }),
            line: diagnostic.range.start.line + 1,
            character: diagnostic.range.start.character,
        }
    }
}

/// The severity of a diagnostic.
#[derive(Clone, Data, PartialEq, Debug)]
pub enum DiagnosticSeverity {
    /// An error.
    Error,
    /// A warning.
    Warning,
    /// An hint or an information.
    Hint,
}

impl From<Option<lsp_types::DiagnosticSeverity>> for DiagnosticSeverity {
    fn from(severity: Option<lsp_types::DiagnosticSeverity>) -> Self {
        match severity {
            Some(lsp_types::DiagnosticSeverity::Hint)
            | Some(lsp_types::DiagnosticSeverity::Information) => DiagnosticSeverity::Hint,
            Some(lsp_types::DiagnosticSeverity::Warning) => DiagnosticSeverity::Warning,
            _ => DiagnosticSeverity::Error,
        }
    }
}

/// The LSP Server state.
#[derive(Clone, Data, Lens)]
pub struct LspServerState {
    /// The language of the LSP server.
    pub language: String,
    /// The current status of the LSP server.
    pub status: LspServerStatus,
    /// The LSP server.
    pub server: Arc<Mutex<LspServer>>,
    /// The diagnostics.
    pub diagnostics: Vector<DiagnosticState>,
}

/// The status of the LSP server.
#[derive(Clone, Data, PartialEq)]
pub enum LspServerStatus {
    /// The LSP server is not started.
    Offline,
    /// The LSP server is connected.
    Online,
}

impl Display for LspServerStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LspServerStatus::Offline => write!(f, "offline"),
            LspServerStatus::Online => write!(f, "online"),
        }
    }
}

/// Gets a new LSP server corresponding to the specified language.  
pub fn get_lsp_server(language_name: &str) -> Option<LspServer> {
    let language_name = language_name.to_owned();
    match language_name.as_str() {
        "Rust" => LspServerBuilder::new("rust-analyzer", &language_name)
            .github_download(
                "rust-analyzer/rust-analyzer",
                "rust-analyzer-windows.exe",
                "rust-analyzer-mac",
                "rust-analyzer-linux",
            )
            .requires(&["cargo"])
            .build(),

        "C++" => LspServerBuilder::new("clangd", &language_name)
            .github_download(
                "clangd/clangd",
                "clangd-windows-",
                "clangd-mac-",
                "clangd-linux-",
            )
            .build(),

        _ => None,
    }
}

/// Builder used to create a LspServer.
#[derive(Default)]
pub struct LspServerBuilder {
    /// The name of this LSP server.
    name: &'static str,
    /// The name of the language targeted by this LSP server.
    language_name: String,
    /// The github repo to download the release from.
    github_url: Option<&'static str>,
    /// A string contained in the windows release.
    windows_matcher: Option<&'static str>,
    /// A string contained in the linux release.
    linux_matcher: Option<&'static str>,
    /// A string contained in the macos release.
    macos_matcher: Option<&'static str>,
    /// Commands required for the LSP server to work.
    required_commands: &'static [&'static str],
}

impl LspServerBuilder {
    /// Creates a new builder for an LSP server
    pub fn new(name: &'static str, language_name: &str) -> Self {
        Self {
            name,
            language_name: language_name.to_owned(),
            ..Self::default()
        }
    }

    /// The server is downloadable through releases in a github repository.
    pub fn github_download(
        mut self,
        github_url: &'static str,
        windows_matcher: &'static str,
        macos_matcher: &'static str,
        linux_matcher: &'static str,
    ) -> Self {
        self.github_url = Some(github_url);
        self.windows_matcher = Some(windows_matcher);
        self.linux_matcher = Some(linux_matcher);
        self.macos_matcher = Some(macos_matcher);
        self
    }

    /// Requires certain commands to be available in the user's PATH.
    /// e.g for pyright we need node, for rust-analyzer we need cargo.
    pub fn requires(mut self, commands: &'static [&'static str]) -> Self {
        self.required_commands = commands;
        self
    }

    /// Builds the LSP server.
    /// Returns None if a required command is not present on the system.
    pub fn build(self) -> Option<LspServer> {
        Some(LspServer {
            id: 0,
            language_name: self.language_name,
            name: self.name,
            outgoing: HashMap::new(),
            process: None,
            github_repo: self.github_url.unwrap(),
            matcher: if cfg!(windows) {
                self.windows_matcher.unwrap()
            } else if cfg!(linux) {
                self.linux_matcher.unwrap()
            } else {
                self.macos_matcher.unwrap()
            },
        })
    }
}

/// An handle to a LSP server process spawned.
struct ProcessHandle {
    /// The LSP server process.
    _process: Child,
    /// The channel used to send messages to the server.
    sender: Sender<Message>,
    /// The channel used to receive messages from the server.
    receiver: Receiver<Message>,
}

/// A generic LSP server.
pub struct LspServer {
    /// The name of the LSP server.
    pub name: &'static str,
    /// The name of the language targeted by this LSP server.
    language_name: String,
    /// The github repository to download the release.
    github_repo: &'static str,
    /// A string contained in this platform's release.
    matcher: &'static str,
    /// The id of the next request to the LSP server.
    id: i32,
    /// The handle to the LSP server process.
    process: Option<ProcessHandle>,
    /// The outgoing Request queue. Every id is associated to a Request/Reponse pair.
    outgoing: HashMap<i32, &'static str>,
}

impl LspServer {
    /// Connects to an LSP server via stdio and sends the initialization
    /// message. Returns false if the LSP server is not downloaded and
    /// thus the connection is impossible.
    pub fn connect(&mut self, root_folder: &Path) -> bool {
        let mut process;
        let path = match LspServer::lsp_path(self.name) {
            Some(path) => path,
            None => return false,
        };

        #[cfg(windows)]
        {
            process = Command::new(path)
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                // on windows hide the lsp server window
                .creation_flags(0x08000000)
                .spawn()
                .unwrap();
        }

        #[cfg(not(windows))]
        {
            process = Command::new("top")
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .spawn()
                .unwrap();
        }

        let mut stdin = process.stdin.take().unwrap();
        let mut stdout = BufReader::new(process.stdout.take().unwrap());

        // get the messages from the LSP server output
        let language_name = self.language_name.clone();
        let (reader_sender, reader_receiver) = unbounded::<Message>();
        thread::spawn(move || {
            let sink = SINK.get().unwrap();
            while let Some(message) = Message::read(&mut stdout) {
                // dbg!(&message);
                reader_sender.send(message).unwrap();
                sink.submit_command(NEW_LSP_MESSAGE, language_name.clone(), Target::Auto)
                    .unwrap();
            }
        });

        let (writer_sender, writer_receiver) = unbounded::<Message>();

        // send the messages to the LSP server input
        thread::spawn(move || {
            writer_receiver.into_iter().for_each(|m| {
                // dbg!(&m);
                m.write(&mut stdin).unwrap();
            });
        });

        self.process = Some(ProcessHandle {
            _process: process,
            sender: writer_sender,
            receiver: reader_receiver,
        });

        #[allow(deprecated)]
        self.send_request(
            Initialize::METHOD,
            InitializeParams {
                process_id: Some(process::id()),
                root_path: None,
                root_uri: Some(Url::from_directory_path(root_folder).unwrap()),
                initialization_options: None,
                capabilities: ClientCapabilities::default(),
                trace: None,
                workspace_folders: None,
                client_info: Some(ClientInfo {
                    name: "ne2".to_owned(),
                    version: Some(env!("CARGO_PKG_VERSION").to_owned()),
                }),
                locale: None,
            },
        );

        true
    }

    /// Gets the LSP server state corresponding to the language of the current file,
    /// if the LSP server is online.
    fn relevant_state(data: &AppState) -> Option<&LspServerState> {
        data.lsp_servers
            .iter()
            .find(|state| state.language == data.editor.language)
            .filter(|state| state.status == LspServerStatus::Online)
    }

    /// Sends to the relevant LSP server a DidOpen notification.
    pub fn did_open(data: &mut AppState) {
        let server_state = match LspServer::relevant_state(data) {
            Some(server_state) => server_state,
            None => return,
        };

        let lsp_server = server_state.server.clone();
        lsp_server.lock().unwrap().send_did_open(data);
    }

    /// Sends a DidOpen notification.
    pub fn send_did_open(&mut self, data: &mut AppState) {
        let file_path = match &data.editor.file {
            Some(file_path) => file_path,
            None => return,
        };

        let language = data.editor.language.clone();
        let server_id = self.id;
        self.send_notification(
            DidOpenTextDocument::METHOD,
            DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Url::from_file_path(file_path).unwrap(),
                    language_id: language,
                    //  use the id as the version number
                    version: server_id,
                    text: data.editor.buffer().to_string(),
                },
            },
        )
    }

    /// Sends to the relevant LSP server a DidClose notification.
    pub fn did_close(data: &mut AppState) {
        let server_state = match LspServer::relevant_state(data) {
            Some(lsp_server) => lsp_server,
            None => return,
        };

        let lsp_server = server_state.server.clone();
        lsp_server.lock().unwrap().send_did_close(data);
    }

    /// Sends a DidClose notification.
    pub fn send_did_close(&mut self, data: &mut AppState) {
        let file_path = match &data.editor.file {
            Some(file_path) => file_path,
            None => return,
        };

        self.send_notification(
            DidCloseTextDocument::METHOD,
            DidCloseTextDocumentParams {
                text_document: TextDocumentIdentifier {
                    uri: Url::from_file_path(file_path).unwrap(),
                },
            },
        );
    }

    /// Sends to the relevant LSP server a DidChange notification.
    pub fn did_change(data: &mut AppState, delta: &DeltaState) {
        let server_state = match LspServer::relevant_state(data) {
            Some(lsp_server) => lsp_server,
            None => return,
        };

        let lsp_server = server_state.server.clone();
        lsp_server.lock().unwrap().send_did_change(data, delta);
    }

    /// Sends a DidChange notification.
    pub fn send_did_change(&mut self, data: &mut AppState, delta: &DeltaState) {
        let file_path = match &data.editor.file {
            Some(file_path) => file_path,
            None => return,
        };

        let old_buffer = delta.old_buffer(&data.editor.engine);
        let (iv, _) = delta.summary();
        let line_start = old_buffer.line_of_offset(iv.start);
        let line_end = old_buffer.line_of_offset(iv.end);
        let line_start_offset = old_buffer.offset_of_line(line_start);
        let line_end_offset = old_buffer.offset_of_line(line_end);

        let char_start = old_buffer.count::<Utf16CodeUnitsMetric>(iv.start)
            - old_buffer.count::<Utf16CodeUnitsMetric>(line_start_offset);

        let char_end = old_buffer.count::<Utf16CodeUnitsMetric>(iv.end)
            - old_buffer.count::<Utf16CodeUnitsMetric>(line_end_offset);

        let server_id = self.id;
        self.send_notification(
            DidChangeTextDocument::METHOD,
            DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    version: server_id,
                    uri: Url::from_file_path(file_path).unwrap(),
                },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: Some(Range {
                        start: Position {
                            line: line_start as u32,
                            character: char_start as u32,
                        },
                        end: Position {
                            line: line_end as u32,
                            character: char_end as u32,
                        },
                    }),
                    range_length: None,
                    text: data.editor.delta.text.to_string(),
                }],
            },
        )
    }

    /// Sends to the relevant LSP server a DidSave notification.
    pub fn did_save(data: &mut AppState) {
        let server_state = match LspServer::relevant_state(data) {
            Some(server_state) => server_state,
            None => return,
        };

        if server_state.status == LspServerStatus::Offline {
            return;
        }

        let file_path = data.editor.file.as_ref().unwrap();
        let mut lsp_server = server_state.server.lock().unwrap();

        lsp_server.send_notification(
            DidSaveTextDocument::METHOD,
            DidSaveTextDocumentParams {
                text_document: TextDocumentIdentifier {
                    uri: Url::from_file_path(file_path).unwrap(),
                },
                text: None,
            },
        )
    }

    /// Sends a Request to the LSP server.
    pub fn send_request<P: Serialize>(&mut self, method: &'static str, params: P) {
        let request = Request::new(self.id, method.to_owned(), params);
        self.outgoing.insert(self.id, method);
        self.process
            .as_mut()
            .unwrap()
            .sender
            .send(request.into())
            .unwrap();
        self.id += 1;
    }

    /// Sends a notification to the LSP server.
    pub fn send_notification<P: Serialize>(&mut self, method: &str, params: P) {
        self.process
            .as_mut()
            .unwrap()
            .sender
            .send(Notification::new(method.to_owned(), params).into())
            .unwrap();
        self.id += 1;
    }

    /// Gets the LSP server path.
    pub fn lsp_path(lsp_name: &str) -> Option<PathBuf> {
        // get the dir where the lsp executable is stored
        let lsp_dir = app_dir(
            AppDataType::UserData,
            &APP_INFO,
            &format!("lsp/{}", lsp_name),
        )
        .unwrap();

        // the first and only file in the dir should be the lsp executable, with a name corresponding
        // to the github release id.
        lsp_dir
            .read_dir()
            .unwrap()
            .next()
            .map(|f| f.unwrap().path())
    }

    /// Checks if the LSP server needs to be updated.
    pub fn check_update(&self) {
        let name = self.name;
        let matcher = self.matcher;
        let repo = self.github_repo;

        thread::spawn(move || {
            let sink = SINK.get().unwrap();
            let lsp_server_id = LspServer::lsp_path(name)
                .map(|f| f.file_name().unwrap().to_string_lossy().to_string());

            // get the latest lsp server executable from github
            let (release_id, download_url) = match LspServer::get_last_release(repo, matcher) {
                Some(info) => info,
                // impossible to get the latest release from github. We're probably offline.
                None if lsp_server_id.is_none() => {
                    sink.submit_command(
                        ADD_TEXT_NOTIFICATION,
                        (
                            None,
                            format!("{} is not installed. You can download it if you turn on your Internet connection.", name)
                        ),
                        Target::Auto,
                    ).unwrap();

                    return;
                }

                None => return,
            };

            let callback: Callback =
                Arc::new(move |(data, notif): &mut (AppState, NotificationState)| {
                    let this = data
                        .notification_center
                        .get_notification(&notif.id)
                        .unwrap();
                    this.button = None;
                    this.text = Arc::from(format!("Downloading {} ...", name));
                    data.download_lsp_server(release_id, download_url.clone());
                });

            // if no lsp server is installed, we aren't up to date
            let lsp_server_id = match lsp_server_id {
                Some(id) => id,
                None => {
                    sink.submit_command(
                        ADD_BUTTON_NOTIFICATION,
                        (
                            Some("ne2.notification.download_lsp_server"),
                            format!("{} is not installed.", name),
                            "Download now".to_owned(),
                            callback,
                        ),
                        Target::Auto,
                    )
                    .unwrap();
                    return;
                }
            };

            if release_id > lsp_server_id.parse().unwrap() {
                sink.submit_command(
                    ADD_BUTTON_NOTIFICATION,
                    (
                        Some("ne2.notification.download_lsp_server"),
                        format!("A new version of {} is available.", name),
                        "Download now".to_owned(),
                        callback,
                    ),
                    Target::Auto,
                )
                .unwrap();
            }
        });
    }

    /// Gets the last release id and the download url for the LSP server of this release.
    fn get_last_release(
        github_repo: &'static str,
        matcher: &'static str,
    ) -> Option<(usize, String)> {
        let github_api_call = format!(
            "https://api.github.com/repos/{}/releases/latest",
            github_repo
        );

        let release: GithubRelease = ureq::get(&github_api_call).call().ok()?.into_json().ok()?;
        release.into_matching_download(matcher)
    }

    /// Gets the state of this LSP server.
    #[inline]
    fn get_state<'a>(&self, data: &'a mut AppState) -> &'a mut LspServerState {
        data.lsp_servers
            .iter_mut()
            .find(|server| server.language == self.language_name)
            .unwrap()
    }

    /// Downloads the LSP server from the provided path.
    pub fn download(&mut self, release_id: usize, download_url: String) {
        let name = self.name;
        thread::spawn(move || {
            let sink = SINK.get().unwrap();

            // start by removing the old files in the folder
            let lsp_dir =
                app_dir(AppDataType::UserData, &APP_INFO, &format!("lsp/{}/", name)).unwrap();

            fs::remove_dir_all(&lsp_dir).unwrap();
            fs::create_dir(&lsp_dir).unwrap();

            match ureq::get(&download_url).call() {
                Ok(data) => {
                    let content_length: f64 =
                        data.header("Content-Length").unwrap().parse().unwrap();
                    let mut cumulated_size = 0.;

                    let mut reader = data.into_reader();
                    let mut lsp_file = File::create(lsp_dir.join(release_id.to_string())).unwrap();

                    let mut middleman = WriterInterceptor {
                        inner: &mut lsp_file,
                        callback: &mut |size: &io::Result<usize>| {
                            cumulated_size += *size.as_ref().unwrap() as f64;
                            sink.submit_command(
                                UPDATE_PROGRESS_BAR,
                                (
                                    "ne2.notification.download_lsp_server",
                                    cumulated_size / content_length,
                                ),
                                Target::Auto,
                            )
                            .unwrap();
                        },
                    };

                    io::copy(&mut reader, &mut middleman).unwrap();
                    sink.submit_command(
                        UPDATE_TEXT,
                        (
                            "ne2.notification.download_lsp_server",
                            Arc::from(format!("{} successfully installed.", name)),
                        ),
                        Target::Auto,
                    )
                    .unwrap();
                }

                Err(err) => {
                    sink.submit_command(
                        ADD_TEXT_NOTIFICATION,
                        (None, format!("Error downloading {}: {}", name, err)),
                        Target::Auto,
                    )
                    .unwrap();
                }
            }
        });
    }
}

/// A middleman to allow callbacks while using io::copy.
pub struct WriterInterceptor<'a> {
    /// The real writer.
    inner: &'a mut dyn Write,
    /// The callback to execute on each write.
    callback: &'a mut dyn FnMut(&io::Result<usize>),
}

impl Write for WriterInterceptor<'_> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let size = self.inner.write(buf);
        (self.callback)(&size);

        size
    }

    fn flush(&mut self) -> io::Result<()> {
        self.inner.flush()
    }
}

/// A github release API response, used to download releases.
#[derive(Deserialize)]
struct GithubRelease {
    /// The id of the release.
    id: usize,
    /// The assets contained in this release.
    assets: Vec<GithubReleaseAsset>,
}

impl GithubRelease {
    /// Gets id of the release and the download url corresponding to the matcher, if any.
    fn into_matching_download(self, matcher: &'static str) -> Option<(usize, String)> {
        let id = self.id;
        self.assets
            .into_iter()
            .find(|asset| asset.name.contains(matcher))
            .map(|asset| (id, asset.browser_download_url))
    }
}

/// A github release asset.
#[derive(Deserialize)]
struct GithubReleaseAsset {
    /// The name of the asset.
    name: String,
    /// The url that can be used to download this asset.
    browser_download_url: String,
}

/// A struct containing the JSON-RPC header.
#[derive(Serialize, Deserialize)]
pub struct JsonRpc {
    /// The JSON-RPC version.
    jsonrpc: &'static str,
    #[serde(flatten)]
    /// The message being transported.
    message: Message,
}

/// A message over JSON-RPC.
/// It can be either a Request, a Response, or a Notification.
#[derive(Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Message {
    /// A request.
    Request(Request),
    /// A response.
    Response(Response),
    /// A notification.
    Notification(Notification),
}

impl Message {
    /// Writes an HTTP-encoded message.
    pub fn write(self, out: &mut impl Write) -> io::Result<()> {
        let payload = serde_json::to_string(&JsonRpc {
            jsonrpc: "2.0",
            message: self,
        })?;
        write!(out, "Content-Length: {}\r\n\r\n{}", payload.len(), payload)?;
        out.flush()
    }

    /// Reads an HTTP-encoded message.
    pub fn read(input: &mut dyn BufRead) -> Option<Self> {
        let mut size = None;
        let mut buf = String::new();

        // parse the headers
        loop {
            buf.clear();
            if input.read_line(&mut buf).ok()? == 0 {
                return None;
            }

            if !buf.ends_with("\r\n") {
                return None;
            }

            let line = &buf[..buf.len() - 2];
            if line.is_empty() {
                break;
            }

            let mut parts = line.splitn(2, ": ");

            if parts.next()? == "Content-Length" {
                size = Some(parts.next()?.parse().ok()?);
            }
        }

        // parse the actual payload
        let mut buf = buf.into_bytes();
        buf.resize(size?, 0);
        input.read_exact(&mut buf).ok()?;
        let message = String::from_utf8(buf).ok()?;

        serde_json::from_str::<Message>(&message).ok()
    }
}

impl From<Request> for Message {
    fn from(request: Request) -> Self {
        Message::Request(request)
    }
}

impl From<Response> for Message {
    fn from(response: Response) -> Self {
        Message::Response(response)
    }
}

impl From<Notification> for Message {
    fn from(notification: Notification) -> Self {
        Message::Notification(notification)
    }
}
/// A request sent by the client.
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Request {
    /// The id of the request.
    id: i32,
    /// The method name.
    method: String,
    /// The params.
    #[serde(default = "serde_json::Value::default")]
    #[serde(skip_serializing_if = "serde_json::Value::is_null")]
    params: serde_json::Value,
}

impl Request {
    /// Creates a new Request.
    pub fn new<P: Serialize>(id: i32, method: String, params: P) -> Self {
        Self {
            id,
            method,
            params: serde_json::to_value(params).unwrap(),
        }
    }

    /// Extracts the request if the method name matches.
    pub fn extract<P: DeserializeOwned>(self, method: &str) -> Result<(i32, P), Request> {
        if self.method == method {
            let params = serde_json::from_value(self.params).unwrap_or_else(|err| {
                panic!("Invalid request\nMethod: {}\n error: {}", method, err)
            });
            Ok((self.id, params))
        } else {
            Err(self)
        }
    }
}

/// A response sent by the server.
#[derive(Debug, Serialize, Deserialize)]
pub struct Response {
    /// The id of the request this response is referring to.
    id: i32,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// The result in case of a success.
    result: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// The error in case of an error.
    error: Option<ResponseError>,
}

/// An error in case the response wasn't successful.
#[derive(Debug, Serialize, Deserialize)]
pub struct ResponseError {
    /// The error code.
    code: i32,
    /// The error message.
    message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// The error data if present.
    data: Option<serde_json::Value>,
}

/// A notification sent by either the server or the client.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Notification {
    /// The method name.
    method: String,
    /// The params.
    #[serde(default = "serde_json::Value::default")]
    #[serde(skip_serializing_if = "serde_json::Value::is_null")]
    params: serde_json::Value,
}

impl Notification {
    /// Creates a new notification.
    pub fn new<P: Serialize>(method: String, params: P) -> Self {
        Self {
            method,
            params: serde_json::to_value(params).unwrap(),
        }
    }

    /// Extracts the notification if the method name matches.
    pub fn extract<P: DeserializeOwned>(self, method: &str) -> Result<P, Notification> {
        if self.method == method {
            let params = serde_json::from_value(self.params).unwrap_or_else(|err| {
                panic!("Invalid notification\nMethod: {}\n error: {}", method, err)
            });
            Ok(params)
        } else {
            Err(self)
        }
    }
}

/// Tries to cast a request into the given type.
fn cast_request<R>(req: Request) -> Result<(i32, R::Params), Request>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

/// Tries to cast a notification into the given type.
fn cast_notification<R>(notif: Notification) -> Result<R::Params, Notification>
where
    R: lsp_types::notification::Notification,
    R::Params: serde::de::DeserializeOwned,
{
    notif.extract(R::METHOD)
}
