use log::trace;
use std::io::{BufRead as _, BufReader, Read};

pub(crate) struct LspReceiver<R> {
    content: Vec<u8>,
    line: String,
    reader: BufReader<R>,
    error_count: usize,
}

impl<R: Read> LspReceiver<R> {
    pub(crate) fn read_next<F: FnMut(&str)>(&mut self, mut f: F) {
        self.line.clear();
        self.reader.read_line(&mut self.line).expect("Read header");
        if !self.line.starts_with("Content-Length:") {
            if self.error_count < 10 {
                self.error_count += 1;
                trace!("Unknown header {}", self.line);
            }
            return;
        }

        let l = "Content-Length:".len();
        let r = self.line.len();
        let content_length = self.line[l..r]
            .trim()
            .parse::<usize>()
            .expect("content length to be integer");

        self.line.clear();
        self.reader.read_line(&mut self.line).expect("Read header");
        if self.line.trim().len() != 0 {
            if self.error_count < 10 {
                self.error_count += 1;
                trace!("Unknown header {}", self.line);
            }
            return;
        }

        self.content.resize(content_length, 0);
        self.reader
            .read_exact(&mut self.content)
            .expect("read payload");

        let json = String::from_utf8_lossy(&self.content);

        trace!("Received {}\n", json);

        f(&json);
    }

    pub(crate) fn new(reader: R) -> Self {
        LspReceiver {
            content: vec![],
            line: String::new(),
            reader: BufReader::new(reader),
            error_count: 0,
        }
    }
}
