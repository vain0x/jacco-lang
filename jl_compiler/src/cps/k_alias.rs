use super::{KModLocalSymbol, KProjectSymbol, KTy2};
use crate::{
    source::Loc,
    utils::{VecArena, VecArenaId},
};

pub(crate) struct KAliasTag;

pub(crate) type KAlias = VecArenaId<KAliasTag>;

pub(crate) type KAliasArena = VecArena<KAliasTag, KAliasOutline>;

#[derive(Clone, Debug)]
pub(crate) struct KAliasOutline {
    name: String,
    path: Vec<String>,
    loc: Loc,
    referent_opt: Option<KProjectSymbol>,
}

impl KAliasOutline {
    pub(crate) fn new(name: String, path: Vec<String>, loc: Loc) -> KAliasOutline {
        KAliasOutline {
            name,
            path,
            loc,
            referent_opt: None,
        }
    }

    pub(crate) fn name(&self) -> &str {
        &self.name
    }

    pub(crate) fn path(&self) -> &[String] {
        &self.path
    }

    pub(crate) fn loc(&self) -> Loc {
        self.loc
    }

    pub(crate) fn referent(&self) -> Option<KProjectSymbol> {
        self.referent_opt
    }

    pub(crate) fn referent_as_ty(&self) -> Option<KTy2> {
        let ty = match self.referent() {
            Some(KProjectSymbol::ModLocal { k_mod, symbol }) => match symbol {
                KModLocalSymbol::ConstEnum(const_enum) => KTy2::ConstEnum(k_mod, const_enum),
                KModLocalSymbol::StructEnum(struct_enum) => KTy2::StructEnum(k_mod, struct_enum),
                _ => return None,
            },
            _ => return None,
        };

        log::trace!("alias referent {} ty={:?}", &self.name, ty);
        Some(ty)
    }

    fn verify_bind(&mut self, referent: KProjectSymbol) -> bool {
        match referent {
            KProjectSymbol::Mod(_) => true,
            KProjectSymbol::ModLocal { symbol, .. } => match symbol {
                KModLocalSymbol::LocalVar { .. } => {
                    log::error!(
                        "エイリアスにはローカル変数をバインドできません {:?}",
                        referent
                    );
                    false
                }
                KModLocalSymbol::Alias(_) => {
                    log::error!("エイリアスにエイリアスをバインドしようとしていますが、無視されます。再エクスポート (pub use) は未実装です {:?}", referent);
                    false
                }
                _ => true,
            },
            _ => true,
        }
    }

    pub(crate) fn bind(&mut self, referent: KProjectSymbol) {
        if !self.verify_bind(referent) {
            return;
        }

        let old_referent = self.referent_opt.replace(referent);
        assert_eq!(old_referent, None);
    }
}
