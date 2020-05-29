use super::*;

#[derive(Clone, Copy, Debug)]
pub(crate) struct KField {
    id: usize,
}

impl KField {
    pub(crate) fn name(self, outlines: &KOutlines) -> &str {
        &outlines.field_get(self).name
    }

    pub(crate) fn ty(self, outlines: &KOutlines) -> &KTy {
        &outlines.field_get(self).ty
    }

    pub(crate) fn location(self, outlines: &KOutlines) -> Location {
        outlines.field_get(self).location.clone()
    }
}

pub(crate) struct KFieldOutline {
    name: String,
    ty: KTy,
    location: Location,
}

impl KFieldOutline {
    pub(crate) fn new(name: String, ty: KTy, location: Location) -> Self {
        Self { name, ty, location }
    }
}

#[derive(Default)]
pub(crate) struct KOutlines {
    pub(crate) fields: Vec<KFieldOutline>,
}

impl KOutlines {
    pub(crate) fn field_new(&mut self, field: KFieldOutline) -> KField {
        let id = self.fields.len();
        self.fields.push(field);
        KField { id }
    }

    pub(crate) fn field_get(&self, field: KField) -> &KFieldOutline {
        &self.fields[field.id]
    }
}
