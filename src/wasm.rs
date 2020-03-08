mod binary;
mod structure;
mod validation;

use crate::{
	print, println,
	wasm::{structure::instructions::Instr, validation::validate},
};
use binary::decode;

pub fn test_wasm() {
	let wasm = include_bytes!("../target/wasm32-unknown-unknown/debug/wasm-test.wasm");
	print!("decoding...");
	let module = decode(wasm);
	print!("validating...");
	validate(&module).unwrap();
	println!("done");
	println!("{}", core::mem::size_of::<Instr>());
}
