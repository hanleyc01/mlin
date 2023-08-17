use std::path::PathBuf;

fn main() {
    println!("cargo:rustc-link-search=/libraries/wrapper.h");
    println!("cargo:rerun-if-changed=/libraries/wrapper.h");
    println!("cargo:rustc-link-lib=mir");

    let bindings = bindgen::Builder::default()
        .header("./libraries/wrapper.h")
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
        .generate()
        .expect("Unable to generate bindings!");

    let out_path = PathBuf::from(std::env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}
