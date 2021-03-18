use super::{collect_def_sites, collect_use_sites, hit_test, Doc, LangService, TPos16, TRange};
use crate::{
    cps::*,
    front::name_resolution::NameSymbol,
    lang_service::{
        doc_analysis::*,
        lang_service::{loc_to_range, to_range16},
    },
    parse::*,
    scope::lexical_referent::LexicalReferent,
    source::Loc,
};
use std::collections::HashMap;

struct FieldOccurrenceInFnCollector<'a> {
    mod_outline: &'a KModOutline,
    fn_data: &'a KFnData,
    occurrences: &'a mut Vec<(KField, Loc)>,
}

impl FieldOccurrenceInFnCollector<'_> {
    fn do_on_field_tag(&mut self, field_tag: &KFieldTag) {
        let field = match field_tag.field_opt {
            Some(it) => it,
            None => return,
        };

        self.occurrences.push((field, field_tag.loc));
    }

    fn on_node(&mut self, node: &KNode) {
        for term in &node.args {
            match term {
                KTerm::FieldTag(field_tag) => self.do_on_field_tag(field_tag),
                _ => {}
            }
        }

        for cont in &node.conts {
            self.on_node(cont);
        }
    }
}

fn collect_field_occurrences(
    only_doc: Option<Doc>,
    ls: &mut LangService,
    occurrences: &mut Vec<(KField, Loc)>,
) {
    ls.request_types();

    for fn_data in ls.mod_data.fns.iter() {
        let mut collector = FieldOccurrenceInFnCollector {
            mod_outline: &ls.mod_outline,
            fn_data,
            occurrences,
        };

        for label_data in fn_data.labels.iter() {
            collector.on_node(&label_data.body);
        }
    }
}

fn hit_test_on_field(doc: Doc, pos: TPos16, ls: &mut LangService) -> Option<PToken> {
    let syntax = ls.request_syntax(doc)?;

    for expr in syntax.tree.ast.exprs().iter() {
        match expr {
            AExpr::Field(AFieldExpr {
                field_opt: Some(field),
                ..
            }) => {
                let range = to_range16(field.range(&syntax.tree.tokens));
                if !range.contains_inclusive(pos) {
                    continue;
                }

                return Some(*field);
            }
            // AExpr::Record(..) => {}
            _ => continue,
        }
    }

    None
}

fn document_highlight_of_fields(
    doc: Doc,
    token: PToken,
    ls: &mut LangService,
) -> Option<(Vec<TRange>, Vec<TRange>)> {
    let mut occurrences = vec![];
    collect_field_occurrences(Some(doc), ls, &mut occurrences);

    let DocSymbolAnalysisMut { syntax, symbols } = ls.request_symbols(doc)?;
    let range = token.range(&syntax.tree.tokens);

    let field = occurrences.iter().find_map(|&(k_field, loc)| {
        let (the_doc, loc) = loc.inner().ok()?;
        if the_doc != doc {
            return None;
        }

        let the_range = loc.range(&syntax.tree).ok()?;
        if the_range != range {
            return None;
        }

        Some(k_field)
    })?;

    // element(kind=Name) -> ast-name
    let mut map = syntax
        .tree
        .ast
        .names()
        .keys()
        .map(|name| {
            let element = name.element(&syntax.tree);
            (element, name)
            // syntax.tree.ast.
        })
        .collect::<HashMap<_, _>>();

    // フィールド式を探す。
    let mut use_sites = occurrences
        .iter()
        .filter(|&&(the_field, _)| the_field == field)
        .map(|&(_, loc)| loc)
        .filter_map(|loc| loc_to_range(loc, doc, &syntax.tree))
        .collect::<Vec<_>>();

    // TODO: field から struct を引く
    let field_name = field.of(&ls.mod_outline.fields).name.clone();
    let struct_opt = ls
        .mod_outline
        .structs
        .enumerate()
        .find_map(|(k_struct, struct_data)| {
            if struct_data.fields.contains(&field) {
                Some(k_struct)
            } else {
                None
            }
        });

    if let Some(k_struct) = struct_opt {
        let DocSymbolAnalysisMut { syntax, symbols } = ls.request_symbols(doc)?;
        let tr = &syntax.tree;

        'z: for element in syntax.tree.elements.iter() {
            if element.kind() == PElementKind::RecordExpr {
                let (record_expr, element) = (element, ());

                // いま注目しているフィールドを含むレコードのレコード式でなければスキップ
                'a: loop {
                    let record_name_opt =
                        record_expr.nth_child_element_of(PElementKind::Name, 0, tr);

                    let name = match record_name_opt {
                        Some(it) => it,
                        None => continue 'z,
                    };

                    let a_name_opt = map.get(&name).cloned();
                    let mut a_name = match a_name_opt {
                        Some(it) => it,
                        None => continue 'z,
                    };

                    if let Some(LexicalReferent::Name(def_name)) =
                        syntax.tree.name_referents.get(&a_name).cloned()
                    {
                        a_name = def_name;
                    }

                    let name_symbol_opt = symbols.name_symbols.get(&a_name);

                    match name_symbol_opt {
                        Some(NameSymbol::ModSymbol(KModSymbol::Struct(the_struct)))
                            if *the_struct == k_struct =>
                        {
                            break 'a
                        }
                        _ => continue 'z,
                    }
                }

                // フィールドをみつける。
                let arg_range = 'arg: loop {
                    for e in record_expr.children() {
                        let e = match e.as_element() {
                            Some(it) => it,
                            None => continue,
                        };

                        let e = e.of(&tr.elements);
                        if e.kind() != PElementKind::Arg {
                            continue;
                        }
                        let label = e.nth_child_element_of(PElementKind::Name, 0, tr)?;
                        let name = label.of(&syntax.tree.elements).first_token(tr)?;
                        if name.text(&tr.tokens) != field_name {
                            continue;
                        }

                        let range = match PLoc::Element(label).range(&syntax.tree) {
                            Ok(it) => it,
                            Err(_) => continue 'z,
                        };
                        break 'arg range;
                    }
                    continue 'z;
                };

                use_sites.push(arg_range);
            }
        }
    }

    // FIXME: 定義箇所も列挙する
    Some((vec![], use_sites))
}

pub(crate) fn document_highlight(
    doc: Doc,
    pos: TPos16,
    ls: &mut LangService,
) -> Option<(Vec<TRange>, Vec<TRange>)> {
    if let Some(token) = hit_test_on_field(doc, pos, ls) {
        return document_highlight_of_fields(doc, token, ls);
    }

    let DocContentAnalysisMut {
        syntax,
        symbols,
        cps,
        mod_outline,
        mod_data,
    } = ls.request_cps(doc)?;

    let (name, _) = hit_test(doc, pos, syntax, symbols, cps, mod_outline, mod_data)?;
    let mut locations = vec![];

    collect_def_sites(
        doc,
        name,
        syntax,
        symbols,
        cps,
        mod_outline,
        mod_data,
        &mut locations,
    );
    let def_sites = locations
        .drain(..)
        .map(|location| location.range())
        .collect();

    collect_use_sites(
        doc,
        name,
        syntax,
        symbols,
        cps,
        mod_outline,
        mod_data,
        &mut locations,
    );
    let use_sites = locations
        .drain(..)
        .map(|location| location.range())
        .collect();

    Some((def_sites, use_sites))
}
