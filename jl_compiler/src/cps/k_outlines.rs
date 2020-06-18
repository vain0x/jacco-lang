use super::{
    k_struct::KStructOutline, KExternFn, KExternFnOutline, KField, KFieldOutline, KFn, KFnOutline,
    KStruct,
};

#[derive(Default)]
pub(crate) struct KOutlines {
    pub(crate) fns: Vec<KFnOutline>,
    pub(crate) extern_fns: Vec<KExternFnOutline>,
    pub(crate) structs: Vec<KStructOutline>,
    pub(crate) fields: Vec<KFieldOutline>,
}

impl KOutlines {
    pub(crate) fn fns_iter(&self) -> impl Iterator<Item = KFn> {
        (0..self.fns.len()).map(KFn::new)
    }

    pub(crate) fn fn_get(&self, k_fn: KFn) -> &KFnOutline {
        &self.fns[k_fn.id()]
    }

    pub(crate) fn fn_get_mut(&mut self, k_fn: KFn) -> &mut KFnOutline {
        &mut self.fns[k_fn.id()]
    }

    pub(crate) fn extern_fns_iter(&self) -> impl Iterator<Item = KExternFn> {
        (0..self.extern_fns.len()).map(KExternFn::new)
    }

    pub(crate) fn extern_fn_get(&self, extern_fn: KExternFn) -> &KExternFnOutline {
        &self.extern_fns[extern_fn.id()]
    }

    pub(crate) fn extern_fn_get_mut(&mut self, extern_fn: KExternFn) -> &mut KExternFnOutline {
        &mut self.extern_fns[extern_fn.id()]
    }

    pub(crate) fn struct_new(&mut self, struct_outline: KStructOutline) -> KStruct {
        let id = self.structs.len();
        self.structs.push(struct_outline);
        KStruct::new(id)
    }

    pub(crate) fn structs_iter(&self) -> impl Iterator<Item = KStruct> {
        (0..self.structs.len()).map(KStruct::new)
    }

    pub(crate) fn struct_get(&self, k_struct: KStruct) -> &KStructOutline {
        &self.structs[k_struct.id()]
    }

    pub(crate) fn field_new(&mut self, field: KFieldOutline) -> KField {
        let id = self.fields.len();
        self.fields.push(field);
        KField::new(id)
    }

    pub(crate) fn field_get(&self, field: KField) -> &KFieldOutline {
        &self.fields[field.id()]
    }
}
