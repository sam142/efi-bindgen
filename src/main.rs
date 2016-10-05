#![feature(rustc_private)]

extern crate clang;
extern crate syntax;

use std::iter::Iterator;
use parser::parse;
use gen::gen_module;

mod types;
mod parser;
mod gen;

fn main() {
    let efi_header = std::env::args().nth(1).expect("No EFI header specified!");
    if let Err(e) = parse(&efi_header).and_then(|module| gen_module(&module).map_err(Box::from)) {
        println!("{}", e.description())
    }
}
