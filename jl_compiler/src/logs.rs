//! エラーや警告などのログ
//!
//! 入力されたソースコードに起因するエラーを報告するのに使う。
//! 処理系のバグなどは log クレートの error! マクロで報告する。

use crate::{
    parse::{PLoc, PTree},
    source::{Doc, HaveLoc, Loc, TRange},
};
use std::{cell::RefCell, mem::take, rc::Rc};

/// 位置情報と関連付けられたエラーメッセージ
#[derive(Clone)]
pub(crate) struct DocLogItem {
    loc: PLoc,
    message: String,
}

impl DocLogItem {
    #[allow(unused)]
    pub(crate) fn into_message(self) -> String {
        self.message
    }

    #[allow(unused)]
    pub(crate) fn range(&self, tree: &PTree) -> Result<TRange, &'static str> {
        self.loc.range(tree)
    }
}

#[derive(Clone, Default)]
pub(crate) struct DocLogs {
    items: Rc<RefCell<Vec<DocLogItem>>>,
}

impl DocLogs {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn logger(&self) -> DocLogger {
        DocLogger {
            parent: self.clone(),
        }
    }

    pub(crate) fn finish(self) -> Vec<DocLogItem> {
        take(&mut self.items.try_borrow_mut().expect("can't share logs"))
    }
}

#[derive(Clone, Default)]
pub(crate) struct DocLogger {
    parent: DocLogs,
}

impl DocLogger {
    pub(crate) fn error(&self, loc: PLoc, message: String) {
        self.parent
            .items
            .borrow_mut()
            .push(DocLogItem { loc, message });
    }
}

/// 位置情報と関連付けられたエラーメッセージ
#[derive(Clone)]
pub(crate) enum LogItem {
    OnLoc { message: String, loc: Loc },
}

impl LogItem {
    pub(crate) fn message(&self) -> &str {
        match self {
            LogItem::OnLoc { message, .. } => message,
        }
    }

    pub(crate) fn loc(&self) -> Loc {
        match self {
            LogItem::OnLoc { loc, .. } => *loc,
        }
    }
}

/// 位置情報と関連付けられたエラーメッセージのコンテナ。
#[derive(Clone, Default)]
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
#[derive(Clone, Default)]
pub(crate) struct Logger {
    parent: Logs,
}

impl Logger {
    pub(crate) fn extend_from_doc_logs(&self, doc: Doc, logs: DocLogs) {
        let mut items = self.parent.inner.borrow_mut();
        for item in logs.finish() {
            let (loc, message) = (item.loc, item.message);
            items.push(LogItem::OnLoc {
                loc: Loc::new(doc, loc),
                message,
            });
        }
    }

    pub(crate) fn error(&self, node: impl HaveLoc, message: String) {
        let loc = node.loc();
        let mut inner = self.parent.inner.borrow_mut();
        inner.push(LogItem::OnLoc { message, loc });
    }
}
