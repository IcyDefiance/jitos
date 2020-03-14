#![no_std]

#[macro_use]
extern crate alloc;
#[macro_use]
extern crate log;

mod binary;
mod exec;
mod syntax;
mod valid;

pub use exec::runtime::Store;
pub use syntax::modules::Module;

#[test]
pub fn test_wasm() {
	let wasm = include_bytes!("../../../../target/wasm32-unknown-unknown/debug/wasm-test.wasm");
	let module = Module::decode(wasm).unwrap();
	module.validate().unwrap();
}
