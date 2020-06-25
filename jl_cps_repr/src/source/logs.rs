//! エラーや警告などのログ
//!
//! 入力されたソースコードに起因するエラーを報告するのに使う。
//! 処理系のバグなどは log クレートの error! マクロで報告する。

use crate::source::{HaveLocation, Location};
use std::cell::RefCell;
use std::mem::take;
use std::rc::Rc;

/// 位置情報と関連付けられたエラーメッセージ
#[derive(Clone, Debug)]
pub struct LogItem {
    pub message: String,
    pub location: Location,
}

/// 位置情報と関連付けられたエラーメッセージのコンテナ。
#[derive(Clone, Debug, Default)]
pub struct Logs {
    inner: Rc<RefCell<Vec<LogItem>>>,
}

impl Logs {
    pub fn new() -> Self {
        Logs::default()
    }

    pub fn is_fatal(&self) -> bool {
        !self.inner.borrow().is_empty()
    }

    pub fn logger(&self) -> Logger {
        Logger {
            parent: self.clone(),
        }
    }

    pub fn finish(self) -> Vec<LogItem> {
        let mut inner = self.inner.try_borrow_mut().expect("can't share logs");
        take(&mut inner)
    }
}

/// ログを出力するもの。
/// 記述を煩雑にしないために、所有権や可変性をごまかしている。(自由にクローンできる。`&self` に書き込める。)
#[derive(Clone, Debug, Default)]
pub struct Logger {
    parent: Logs,
}

impl Logger {
    pub fn error(&self, have_location: &impl HaveLocation, message: impl Into<String>) {
        let message = message.into();
        let location = have_location.location();
        let mut inner = self.parent.inner.borrow_mut();
        inner.push(LogItem { message, location });
    }
}
