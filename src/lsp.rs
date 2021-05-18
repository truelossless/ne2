//! A module handling Language Server Protocol interactions.
//!
//! A good chunk of the code have been adapted from https://github.com/rust-analyzer/lsp-server.
//! I can't really use directly this crate as it's more for servers than for clients.

use app_dirs2::{app_dir, AppDataType};
use crossbeam_channel::{unbounded, Receiver, Sender};
use serde::{Deserialize, Serialize};
use std::{
    fs::{self, File},
    io::{self, BufRead, BufReader, Write},
    process::{Child, Command, Stdio},
    sync::Arc,
    thread,
};

#[cfg(windows)]
use std::os::windows::process::CommandExt;

use crate::{
    app::{AppState, SINK},
    notification_center::{
        Callback, NotificationState, ADD_BUTTON_NOTIFICATION, ADD_TEXT_NOTIFICATION,
        NOTIFICATION_CENTER_ID, UPDATE_PROGRESS_BAR, UPDATE_TEXT,
    },
    settings::APP_INFO,
};

/// Gets a new LSP server corresponding to the specified language.  
pub fn get_lsp_server(language_name: &str) -> Option<LspServer> {
    match language_name {
        "Rust" => Some(
            LspParams {
                name: "rust-analyzer",
                github_url: "rust-analyzer/rust-analyzer",
                windows_matcher: "rust-analyzer-windows.exe",
                linux_matcher: "rust-analyzer-linux",
                macos_matcher: "rust-analyzer-mac",
                connection: ConnectionMethod::Stdio,
            }
            .into(),
        ),

        "C++" => Some(
            LspParams {
                name: "clangd",
                github_url: "clangd/clangd",
                windows_matcher: "clangd-windows-",
                linux_matcher: "clangd-linux-",
                macos_matcher: "clangd-mac-",
                connection: ConnectionMethod::Stdio,
            }
            .into(),
        ),
        _ => None,
    }
}

/// Parameters used to create a LspServer.
pub struct LspParams {
    /// The name of this LSP server.
    name: &'static str,
    /// The github repo to download the release from.
    github_url: &'static str,
    /// A string contained in the windows release.
    windows_matcher: &'static str,
    /// A string contained in the linux release.
    linux_matcher: &'static str,
    /// A string contained in the macos release.
    macos_matcher: &'static str,
    /// The connection method.
    connection: ConnectionMethod,
}

/// The connection method to an LSP server.
pub enum ConnectionMethod {
    /// Connection via stdio.
    Stdio,
    /// Connection via sockets.
    Socket(u16),
}

impl From<LspParams> for LspServer {
    fn from(params: LspParams) -> Self {
        let matcher = if cfg!(windows) {
            params.windows_matcher
        } else if cfg!(linux) {
            params.linux_matcher
        } else {
            params.macos_matcher
        };

        match params.connection {
            ConnectionMethod::Stdio => {
                LspServer::new_stdio(params.name, params.github_url, matcher)
            }
            ConnectionMethod::Socket(_) => unimplemented!(),
        }
    }
}

/// A generic LSP server.
pub struct LspServer {
    /// The name of the LSP server.
    pub name: &'static str,
    /// The github repository to download the release.
    github_repo: &'static str,
    /// A string contained in this platform's release.
    matcher: &'static str,
    /// The id of the next request to the LSP server.
    id: u32,
    /// The handle to the LSP server process.
    process: Child,
    /// The channel used to send messages to the server.
    pub sender: Sender<Message>,
    /// The channel used to receive messages from the server.
    pub receiver: Receiver<Message>,
}

impl LspServer {
    /// Connects to an LSP server via stdio.
    pub fn new_stdio(name: &'static str, github_repo: &'static str, matcher: &'static str) -> Self {
        let mut process;
        #[cfg(windows)]
        {
            process = Command::new("d:/desktop/rust-analyzer-windows.exe")
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

        let (reader_sender, reader_receiver) = unbounded::<Message>();
        thread::spawn(move || {
            // get the messages from the LSP server output
            // let mut buf = String::new();
            // loop {
            //     stdout.read_line(&mut buf).unwrap();

            //     if !buf.is_empty() {
            //         dbg!(&buf);
            //     }
            // }

            while let Some(message) = Message::read(&mut stdout) {
                // dbg!(&message);
                reader_sender.send(message).unwrap();
            }
        });

        let (writer_sender, writer_receiver) = unbounded::<Message>();
        thread::spawn(move || {
            // let mut fake_stream = Vec::new();

            // send the messages to the LSP server input
            writer_receiver.into_iter().for_each(|m| {
                // fake_stream.clear();
                // m.write(&mut fake_stream).unwrap();
                // dbg!(&m);
                m.write(&mut stdin).unwrap();
                // dbg!(String::from_utf8_lossy(&fake_stream));
            });
        });

        Self {
            name,
            github_repo,
            matcher,
            id: 0,
            process,
            sender: writer_sender,
            receiver: reader_receiver,
        }
    }

    // /// Connects to an LSP server via a socket.
    // pub fn new_socket(binary_url: &'static str, ip: Into<Add) {
    //     unimplemented!();
    // }

    /// Sends a Request to the LSP server.
    pub fn send<P: Serialize>(&mut self, method: &str, params: P) {
        self.sender
            .send(Request::new(self.id, method.to_owned(), params).into())
            .unwrap();
        self.id += 1;
    }

    /// Checks if the LSP server needs to be updated.
    pub fn check_update(&self) {
        let name = self.name;
        let matcher = self.matcher;
        let repo = self.github_repo;

        thread::spawn(move || {
            let sink = SINK.get().unwrap();

            // get the dir where the lsp executable is stored
            let lsp_dir =
                app_dir(AppDataType::UserData, &APP_INFO, &format!("lsp/{}", name)).unwrap();

            // the first and only file in the dir should be the lsp executable, with a name corresponding
            // to the github release id.
            let lsp_server_id = lsp_dir
                .read_dir()
                .unwrap()
                .next()
                .map(|f| f.unwrap().file_name());

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
                        NOTIFICATION_CENTER_ID,
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
                    data.editor
                        .download_lsp_server(release_id, download_url.clone());
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
                        NOTIFICATION_CENTER_ID,
                    )
                    .unwrap();
                    return;
                }
            };

            if release_id > lsp_server_id.to_string_lossy().parse().unwrap() {
                sink.submit_command(
                    ADD_BUTTON_NOTIFICATION,
                    (
                        Some("ne2.notification.download_lsp_server"),
                        format!("A new version of {} is available.", name),
                        "Download now".to_owned(),
                        callback,
                    ),
                    NOTIFICATION_CENTER_ID,
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
                                NOTIFICATION_CENTER_ID,
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
                        NOTIFICATION_CENTER_ID,
                    )
                    .unwrap();
                }

                Err(err) => {
                    sink.submit_command(
                        ADD_TEXT_NOTIFICATION,
                        (None, format!("Error downloading {}: {}", name, err)),
                        NOTIFICATION_CENTER_ID,
                    )
                    .unwrap();
                }
            }
        });
    }
}

// make sure the resources are correctly released.
impl Drop for LspServer {
    fn drop(&mut self) {
        self.process.kill().unwrap();
        self.process.wait().unwrap();
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
#[derive(Debug, Serialize, Deserialize)]
pub struct Request {
    /// The id of the request.
    id: u32,
    /// The method name.
    method: String,
    /// The params.
    #[serde(default = "serde_json::Value::default")]
    #[serde(skip_serializing_if = "serde_json::Value::is_null")]
    params: serde_json::Value,
}

impl Request {
    /// Creates a new Request.
    pub fn new<P: Serialize>(id: u32, method: String, params: P) -> Request {
        Request {
            id,
            method,
            params: serde_json::to_value(params).unwrap(),
        }
    }
}

/// A response sent by the server.
#[derive(Debug, Serialize, Deserialize)]
pub struct Response {
    /// The id of the request this response is referring to.
    id: u32,
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
#[derive(Debug, Serialize, Deserialize)]
pub struct Notification {
    /// The method name.
    method: String,
    /// The params.
    #[serde(default = "serde_json::Value::default")]
    #[serde(skip_serializing_if = "serde_json::Value::is_null")]
    params: serde_json::Value,
}

// /// Tries to cast a request into the given type.
// https://github.com/rust-analyzer/lsp-server/blob/master/examples/goto_def.rs
// fn cast<R>(req: Request) -> Result<(RequestId, R::Params), Request>
// where
//     R: lsp_types::request::Request,
//     R::Params: serde::de::DeserializeOwned,
// {
//     req.extract(R::METHOD)
// }

// #[cfg(test)]
// mod tests {

//     const DECODED: &str = "{ \"key1\": 12\n\"key2\": 345 }";
//     const ENCODED: &str = "Content-Length: 26\r\n\r\n{ \"key1\": 12\n\"key2\": 345 }";

//     use io::BufReader;

//     use super::*;
//     #[test]
//     fn payload_http_encode() {
//         let mut encoded = Vec::new();
//         http_encode(&mut encoded, DECODED).unwrap();
//         assert_eq!(encoded, ENCODED.as_bytes());
//     }

//     #[test]
//     fn payload_http_decode() {
//         let mut reader = BufReader::new(ENCODED.as_bytes());
//         let decoded = http_decode(&mut reader).unwrap();
//         assert_eq!(&decoded, DECODED);
//     }
// }
