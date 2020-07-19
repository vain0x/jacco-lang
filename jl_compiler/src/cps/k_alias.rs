use super::{KModLocalSymbol, KProjectSymbol};
use crate::{
    token::Location,
    utils::{VecArena, VecArenaId},
};

pub(crate) struct KAliasTag;

pub(crate) type KAlias = VecArenaId<KAliasTag>;

pub(crate) type KAliasArena = VecArena<KAliasTag, KAliasOutline>;

#[derive(Clone, Debug)]
pub(crate) struct KAliasOutline {
    name: String,
    path: Vec<String>,
    location: Location,
    referent_opt: Option<KProjectSymbol>,
}

impl KAliasOutline {
    pub(crate) fn new(name: String, path: Vec<String>, location: Location) -> KAliasOutline {
        KAliasOutline {
            name,
            path,
            location,
            referent_opt: None,
        }
    }

    #[allow(unused)]
    pub(crate) fn name(&self) -> &str {
        &self.name
    }

    pub(crate) fn path(&self) -> &[String] {
        &self.path
    }

    pub(crate) fn location(&self) -> Location {
        self.location
    }

    pub(crate) fn referent(&self) -> Option<KProjectSymbol> {
        self.referent_opt
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
