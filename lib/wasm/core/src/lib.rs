#![no_std]

#[macro_use]
extern crate alloc;
#[macro_use]
extern crate log;

pub mod exec;
pub mod syntax;

mod binary;
mod valid;

#[test]
pub fn test_wasm() {
	use syntax::modules::Module;

	let wasm = include_bytes!("../../../../target/wasm32-wasi/debug/wasm-test.wasm");
	let mut module = Module::decode(wasm).unwrap();
	module.validate().unwrap();
}
