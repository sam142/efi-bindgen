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
    match parse(&efi_header) {
        Err(e) => println!("{}", e.description()),
        Ok(module) => gen_module(&module),
    }
}
