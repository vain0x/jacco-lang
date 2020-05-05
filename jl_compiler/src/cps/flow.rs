#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum JumpTarget {
    /// break or continue
    Loop,
    /// return
    Fn,
}

pub(crate) type Flow = Result<(), JumpTarget>;

pub(crate) const SEQUENTIAL: Flow = Ok(());

pub(crate) const LOOP: Flow = Err(JumpTarget::Loop);

pub(crate) const FN: Flow = Err(JumpTarget::Fn);

pub(crate) fn end_loop(flow: &mut Flow) {
    if *flow == LOOP {
        *flow = SEQUENTIAL;
    }
}

pub(crate) fn join(first: Flow, second: Flow) -> Flow {
    match (first, second) {
        (SEQUENTIAL, _) | (_, SEQUENTIAL) => SEQUENTIAL,
        (LOOP, _) | (_, LOOP) => LOOP,
        (FN, FN) => FN,
    }
}
