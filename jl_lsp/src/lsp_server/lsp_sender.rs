use super::{LspNotification, LspResponse};
use log::trace;
use serde::Serialize;
use std::io::{BufWriter, Write};

pub(crate) struct LspSender<W: Write> {
    writer: BufWriter<W>,
}

impl<W: Write> LspSender<W> {
    pub(crate) fn new(writer: W) -> LspSender<W> {
        LspSender {
            writer: BufWriter::new(writer),
        }
    }

    fn do_send(&mut self, content: &[u8]) {
        let content_length = content.len();
        let content = String::from_utf8_lossy(content);

        write!(
            self.writer,
            "Content-Length: {}\r\n\r\n{}",
            content_length, content
        )
        .unwrap();
        self.writer.flush().unwrap();

        trace!(
            "lsp-sender/send Content-Length: {}\r\n\r\n{}",
            content_length,
            content
        );
    }

    pub(crate) fn send_notification<P: Serialize>(&mut self, method: &str, params: P) {
        let mut buf = Vec::new();
        serde_json::to_writer(
            &mut buf,
            &LspNotification::<P> {
                jsonrpc: "2.0".to_string(),
                method: method.to_string(),
                params,
            },
        )
        .unwrap();

        self.do_send(&buf);
    }

    pub(crate) fn send_response<R: Serialize>(&mut self, id: i64, result: R) {
        let mut buf = Vec::new();
        serde_json::to_writer(
            &mut buf,
            &LspResponse::<R> {
                jsonrpc: "2.0".to_string(),
                id,
                result,
            },
        )
        .unwrap();

        self.do_send(&buf);
    }
}
