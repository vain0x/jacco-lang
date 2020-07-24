use std::fmt::{self, Debug, Formatter};

pub(crate) trait DebugWithContext<Context> {
    /// 他のオブジェクトへの参照を使いつつ、デバッグ出力を行う。
    fn fmt(&self, context: &Context, f: &mut Formatter<'_>) -> fmt::Result;
}

/// デバッグ出力する値と、デバッグ出力に必要なオブジェクトのペア。
pub(crate) struct DebugWith<'a, T, Context> {
    value: &'a T,
    context: &'a Context,
}

impl<'a, T, Context> DebugWith<'a, T, Context> {
    pub(crate) fn new(value: &'a T, context: &'a Context) -> Self {
        Self { value, context }
    }
}

impl<'a, T, Context> Debug for DebugWith<'a, T, Context>
where
    T: DebugWithContext<Context>,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        DebugWithContext::fmt(self.value, self.context, f)
    }
}
