use super::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct KFn {
    id: usize,
}

#[derive(Clone, Debug)]
pub(crate) struct KFnOutline {
    pub(crate) name: String,
    pub(crate) param_tys: Vec<KTy>,
    pub(crate) result_ty: KTy,
    pub(crate) location: Location,
}

impl KFn {
    pub(crate) fn id(self) -> usize {
        self.id
    }

    pub(crate) fn param_tys(self, outlines: &KOutlines) -> &[KTy] {
        &outlines.fn_get(self).param_tys
    }

    pub(crate) fn result_ty(self, outlines: &KOutlines) -> &KTy {
        &outlines.fn_get(self).result_ty
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct KExternFn {
    id: usize,
}

impl KExternFn {
    pub(crate) fn id(self) -> usize {
        self.id
    }

    pub(crate) fn param_tys(self, outlines: &KOutlines) -> &[KTy] {
        &outlines.extern_fn_get(self).param_tys
    }

    pub(crate) fn result_ty(self, outlines: &KOutlines) -> &KTy {
        &outlines.extern_fn_get(self).result_ty
    }
}

#[derive(Clone, Debug)]
pub(crate) struct KExternFnOutline {
    pub(crate) name: String,
    pub(crate) param_tys: Vec<KTy>,
    pub(crate) result_ty: KTy,
    pub(crate) location: Location,
}

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
    pub(crate) fns: Vec<KFnOutline>,
    pub(crate) extern_fns: Vec<KExternFnOutline>,
    pub(crate) structs: Vec<KStructOutline>,
    pub(crate) fields: Vec<KFieldOutline>,
}

impl KOutlines {
    pub(crate) fn fn_new(&mut self, fn_outline: KFnOutline) -> KFn {
        let id = self.fns.len();
        self.fns.push(fn_outline);
        KFn { id }
    }

    pub(crate) fn fns_iter(&self) -> impl Iterator<Item = KFn> {
        (0..self.fns.len()).map(|id| KFn { id })
    }

    pub(crate) fn fn_get(&self, k_fn: KFn) -> &KFnOutline {
        &self.fns[k_fn.id]
    }

    pub(crate) fn extern_fn_new(&mut self, extern_fn_outline: KExternFnOutline) -> KExternFn {
        let id = self.extern_fns.len();
        self.extern_fns.push(extern_fn_outline);
        KExternFn { id }
    }

    pub(crate) fn extern_fns_iter(&self) -> impl Iterator<Item = KExternFn> {
        (0..self.extern_fns.len()).map(|id| KExternFn { id })
    }

    pub(crate) fn extern_fn_get(&self, extern_fn: KExternFn) -> &KExternFnOutline {
        &self.extern_fns[extern_fn.id]
    }

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
