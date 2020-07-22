use std::fmt::Debug;

pub(crate) struct DynError(Box<dyn Debug + Send + Sync + 'static>);

impl DynError {
    pub(crate) fn into_inner(self) -> Box<dyn Debug + Send + Sync + 'static> {
        self.0
    }
}

impl<T: Debug + Send + Sync + 'static> From<T> for DynError {
    fn from(value: T) -> Self {
        Self(Box::new(value))
    }
}
