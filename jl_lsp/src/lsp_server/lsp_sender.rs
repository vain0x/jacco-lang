use crate::utils::json::*;
use log::trace;
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

    fn do_send(&mut self, content: &str) {
        let content_length = content.len();

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

    pub(crate) fn send_notification(&mut self, method: &str, params: JsonValue) {
        let obj = {
            let mut o = JsonObject::new();
            o.insert("jsonrpc").string("2.0");
            o.insert("method").string(method);
            o.insert("params").value(params);
            o
        };

        self.do_send(&JsonValue::Object(obj).stringify());
    }

    pub(crate) fn send_response(&mut self, id: i64, result: JsonValue) {
        let obj = {
            let mut o = JsonObject::new();
            o.insert("jsonrpc").string("2.0");
            o.insert("id").number(id as f64);
            o.insert("result").value(result);
            o
        };

        self.do_send(&JsonValue::Object(obj).stringify());
    }
}
