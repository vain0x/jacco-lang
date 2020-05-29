use super::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct KStruct {
    id: usize,
}

impl KStruct {
    pub(crate) fn id(self) -> usize {
        self.id
    }

    pub(crate) fn name(self, outlines: &KOutlines) -> &str {
        &outlines.struct_get(self).name
    }

    pub(crate) fn fields(self, outlines: &KOutlines) -> &[KField] {
        &outlines.struct_get(self).fields
    }
}

#[derive(Clone, Debug)]
pub(crate) struct KStructOutline {
    pub(crate) name: String,
    pub(crate) fields: Vec<KField>,
    pub(crate) location: Location,
}

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
    pub(crate) structs: Vec<KStructOutline>,
    pub(crate) fields: Vec<KFieldOutline>,
}

impl KOutlines {
    pub(crate) fn struct_new(&mut self, struct_outline: KStructOutline) -> KStruct {
        let id = self.structs.len();
        self.structs.push(struct_outline);
        KStruct { id }
    }

    pub(crate) fn structs_iter(&self) -> impl Iterator<Item = KStruct> {
        (0..self.structs.len()).map(|id| KStruct { id })
    }

    pub(crate) fn struct_get(&self, k_struct: KStruct) -> &KStructOutline {
        &self.structs[k_struct.id]
    }

    pub(crate) fn field_new(&mut self, field: KFieldOutline) -> KField {
        let id = self.fields.len();
        self.fields.push(field);
        KField { id }
    }

    pub(crate) fn field_get(&self, field: KField) -> &KFieldOutline {
        &self.fields[field.id]
    }
}
