use std::fmt::Debug;

pub(crate) struct DynError(Box<dyn Debug + 'static>);

impl DynError {
    pub(crate) fn into_inner(self) -> Box<dyn Debug + 'static> {
        self.0
    }
}

impl<T: Debug + 'static> From<T> for DynError {
    fn from(value: T) -> Self {
        Self(Box::new(value))
    }
}
