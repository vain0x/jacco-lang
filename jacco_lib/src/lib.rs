pub fn compile(source_path: &std::path::Path, source_code: &str) -> String {
    use log::{error, trace};
    use std::rc::Rc;

    trace!("source_path = {:?}", source_path);

    let logs = logs::Logs::new();

    let source_path = Rc::new(std::path::PathBuf::from(source_path));
    let source_file = source::SourceFile { source_path };
    let token_source = token::TokenSource::File(source_file);
    let source_code = Rc::new(source_code.to_string());
    let tokens = token::tokenize(token_source, source_code);
    trace!("tokens = {:#?}\n", tokens);

    let mut p_root = parse::parse_tokens(tokens, logs.logger());

    front::validate_syntax(&p_root, logs.logger());
    if logs.is_fatal() {
        for item in logs.finish() {
            error!("{:?} {}", item.location, item.message);
        }
        return String::new();
    }

    let name_resolution = front::resolve_name(&mut p_root, logs.logger());
    trace!("p_root = {:#?}\n", p_root);

    let mut k_root = cps::cps_conversion(p_root, name_resolution, logs.logger());
    trace!("k_root (gen) = {:#?}\n", k_root);

    cps::eliminate_unit(&mut k_root);
    trace!("k_root (elim) = {:#?}\n", k_root);

    for item in logs.finish() {
        error!("{:?} {}", item.location, item.message);
    }

    clang::clang_dump(k_root)
}
