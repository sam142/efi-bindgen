extern crate clang;

use std::iter::Iterator;
use parser::parse;

mod types;
mod parser;

fn main() {
    let efi_header = std::env::args().nth(1).expect("No EFI header specified!");
    match parse(&efi_header) {
        Err(e) => println!("{}", e.description()),
        Ok(module) => println!("{:?}", module),
    }
}
