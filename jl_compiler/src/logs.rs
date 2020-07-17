//! エラーや警告などのログ
//!
//! 入力されたソースコードに起因するエラーを報告するのに使う。
//! 処理系のバグなどは log クレートの error! マクロで報告する。

use crate::{
    source::{loc::LocResolver, TRange},
    token::{HaveLocation, Location, TokenSource},
};
use std::{cell::RefCell, mem::take, path::PathBuf, rc::Rc};

/// 位置情報と関連付けられたエラーメッセージ
#[derive(Clone, Debug)]
pub(crate) enum LogItem {
    OnLocation { message: String, location: Location },
}

impl LogItem {
    pub(crate) fn message(&self) -> &str {
        match self {
            LogItem::OnLocation { message, .. } => message,
        }
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            LogItem::OnLocation { location, .. } => *location,
        }
    }

    pub(crate) fn resolve(self, resolver: &impl LocResolver) -> (String, Option<PathBuf>, TRange) {
        match self {
            LogItem::OnLocation { message, location } => {
                let path_opt = match location.source {
                    TokenSource::Special(name) => Some(PathBuf::from(name)),
                    TokenSource::File(doc) => resolver.doc_path(doc).map(PathBuf::from),
                };
                let range = TRange::from(location.range());
                (message, path_opt, range)
            }
        }
    }
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
        inner.push(LogItem::OnLocation { message, location });
    }

    pub(crate) fn unexpected(&self, have_location: impl HaveLocation, message: impl Into<String>) {
        self.error(have_location, format!("[バグ?] {}", message.into()))
    }

    pub(crate) fn unimpl(&self, have_location: impl HaveLocation, message: impl Into<String>) {
        self.error(have_location, format!("[未実装?] {}", message.into()))
    }
}
