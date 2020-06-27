use super::{
    k_struct::KStructOutline, KConstData, KExternFnOutline, KFieldOutline, KFnOutline,
    KStaticVarData, KStruct,
};

#[derive(Debug, Default)]
pub(crate) struct KOutlines {
    pub(crate) consts: Vec<KConstData>,
    pub(crate) static_vars: Vec<KStaticVarData>,
    pub(crate) fns: Vec<KFnOutline>,
    pub(crate) extern_fns: Vec<KExternFnOutline>,
    pub(crate) structs: Vec<KStructOutline>,
    pub(crate) fields: Vec<KFieldOutline>,
}

impl KOutlines {
    pub(crate) fn struct_new(&mut self, struct_outline: KStructOutline) -> KStruct {
        let id = self.structs.len();
        self.structs.push(struct_outline);
        KStruct::new(id)
    }

    pub(crate) fn struct_get(&self, k_struct: KStruct) -> &KStructOutline {
        &self.structs[k_struct.id()]
    }
}
