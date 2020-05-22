//! エラーや警告などのログ

use crate::token::{HaveLocation, Location};
use std::cell::RefCell;
use std::mem::take;
use std::rc::Rc;

/// 位置情報と関連付けられたエラーメッセージ
#[derive(Clone, Debug)]
pub(crate) struct LogItem {
    pub(crate) message: String,
    pub(crate) location: Location,
}

/// 位置情報と関連付けられたエラーメッセージのコンテナ。
#[derive(Clone, Debug, Default)]
pub(crate) struct Logs {
    inner: Rc<RefCell<Vec<LogItem>>>,
}

impl Logs {
    pub(crate) fn new() -> Self {
        Logs::default()
    }

    pub(crate) fn is_fatal(&self) -> bool {
        !self.inner.borrow().is_empty()
    }

    pub(crate) fn logger(&self) -> Logger {
        Logger {
            parent: self.clone(),
        }
    }

    pub(crate) fn finish(self) -> Vec<LogItem> {
        let mut inner = self.inner.try_borrow_mut().expect("can't share logs");
        take(&mut inner)
    }
}

/// ログを出力するもの。
/// 記述を煩雑にしないために、所有権や可変性をごまかしている。(自由にクローンできる。`&self` に書き込める。)
#[derive(Clone, Debug, Default)]
pub(crate) struct Logger {
    parent: Logs,
}

impl Logger {
    pub(crate) fn error(&self, have_location: impl HaveLocation, message: impl Into<String>) {
        let message = message.into();
        let location = have_location.location();
        let mut inner = self.parent.inner.borrow_mut();
        inner.push(LogItem { message, location });
    }
}
