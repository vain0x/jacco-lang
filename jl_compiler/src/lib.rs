mod clang;
mod cps;
mod front;
mod logs;
mod parse;
mod source;
mod token;

pub fn compile(source_path: &std::path::Path, source_code: &str) -> String {
    eprintln!("{:?}", source_path);

    let source_path = std::rc::Rc::new(std::path::PathBuf::from(source_path));
    let source_file = source::SourceFile { source_path };
    let token_source = token::TokenSource::File(source_file);
    let source_code = std::rc::Rc::new(source_code.to_string());
    let tokens = token::tokenize(token_source, source_code);
    eprintln!("tokens = {:#?}\n", tokens);

    let mut p_root = parse::parse_tokens(tokens);
    front::resolve_name(&mut p_root);
    eprintln!("p_root = {:#?}\n", p_root);

    let k_root = cps::cps_conversion(p_root);
    eprintln!("k_root = {:#?}\n", k_root);

    clang::clang_dump(k_root)
}
