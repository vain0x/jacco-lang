use crate::{
    cps::{KAliasOutline, KModOutline},
    parse::{AName, AUseDecl},
    source::Loc,
};

pub(crate) fn register_use_decl(decl: &AUseDecl, mod_outline: &mut KModOutline) {
    let (name, path) = match &decl.name_opt {
        Some(AName { text, full_name }) => (
            text.to_string(),
            full_name.split("::").map(|part| part.to_string()).collect(),
        ),
        None => Default::default(),
    };

    mod_outline
        .aliases
        .alloc(KAliasOutline::new(name, path, Loc::Unknown("<alias>")));
}
