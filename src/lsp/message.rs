//! Here is all the incoming messages handling.
//! `LspServer::handle_new_message` acts as a bridge between
//! the LSP server and the editor.

use lsp_types::{
    notification::{Initialized, Notification, PublishDiagnostics},
    request::{Initialize, Request},
    InitializedParams,
};

use crate::app::AppState;

use super::{cast_notification, DiagnosticState, LspServer, LspServerStatus, Message};

impl LspServer {
    /// Handles a message sent by the LSP server.
    pub fn handle_new_message(&mut self, data: &mut AppState) {
        match self.process.as_mut().unwrap().receiver.recv().unwrap() {
            Message::Response(res) => {
                let req = self.outgoing.remove(&res.id).unwrap();
                if req == Initialize::METHOD {
                    self.get_state(data).status = LspServerStatus::Online;
                    self.send_notification(Initialized::METHOD, InitializedParams {});
                    self.send_did_open(data);
                }
            }

            Message::Notification(notif) => {
                if let Ok(params) = cast_notification::<PublishDiagnostics>(notif) {
                    self.get_state(data).diagnostics = params
                        .diagnostics
                        .into_iter()
                        .map(DiagnosticState::from)
                        .collect();
                }
            }
            _ => (),
        }
    }
}
