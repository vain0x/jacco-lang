use super::KTy;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct KLocal {
    id: usize,
}

impl KLocal {
    pub(crate) fn new(id: usize) -> Self {
        Self { id }
    }

    pub fn id(self) -> usize {
        self.id
    }

    pub fn name(self, locals: &[KLocalData]) -> &str {
        &locals[self.id].name
    }

    pub fn ty(self, locals: &[KLocalData]) -> &KTy {
        &locals[self.id].ty
    }

    pub fn ty_mut(self, locals: &mut [KLocalData]) -> &mut KTy {
        &mut locals[self.id].ty
    }
}

#[derive(Clone, Debug, Default)]
pub struct KLocalData {
    pub(crate) name: String,
    pub(crate) ty: KTy,
    pub(crate) is_alive: bool,
}
