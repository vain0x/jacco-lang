use crate::token::Location;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub(crate) struct LogItem {
    pub(crate) message: String,
    pub(crate) location: Location,
}

/// Collection of info/warn/error messages
/// associated to source locations.
#[derive(Clone, Debug, Default)]
pub(crate) struct Logs {
    inner: Rc<RefCell<Vec<LogItem>>>,
}

impl Logs {
    pub(crate) fn new() -> Self {
        Logs::default()
    }

    pub(crate) fn logger(&self) -> Logger {
        Logger {
            parent: self.clone(),
        }
    }

    pub(crate) fn finish(self) -> Vec<LogItem> {
        let mut inner = self.inner.try_borrow_mut().expect("can't share logs");
        std::mem::take(&mut inner)
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct Logger {
    parent: Logs,
}

impl Logger {
    pub(crate) fn error(&self, location: Location, message: impl Into<String>) {
        let message = message.into();
        let mut inner = self.parent.inner.borrow_mut();
        inner.push(LogItem { message, location });
    }
}
