fn main() {
    let manifest_dir: &'static str = env!("CARGO_MANIFEST_DIR");

    println!("cargo:rustc-link-search={}", manifest_dir);
}
