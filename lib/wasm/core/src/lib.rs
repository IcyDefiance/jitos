#![no_std]

#[macro_use]
extern crate alloc;
#[macro_use]
extern crate log;

mod binary;
mod structure;
mod validation;

pub use binary::module_decode;
pub use validation::module_validate;

#[test]
pub fn test_wasm() {
	let wasm = include_bytes!("../../../../target/wasm32-unknown-unknown/debug/wasm-test.wasm");
	let module = module_decode(wasm).unwrap();
	module_validate(&module).unwrap();
}
