mod jitos_env;

use alloc::vec::Vec;
use core::str::FromStr;
use cranelift_codegen::{
	ir::function::DisplayFunctionAnnotations,
	isa,
	settings::{self, Configurable},
	write_function,
};
use cranelift_wasm::translate_module;
use jitos_env::JitosEnvironment;
use target_lexicon::triple;

pub fn test_wasm() {
	let wasm = include_bytes!("../target/wasm32-unknown-unknown/debug/wasm-test.wasm");
	let mut env = JitosEnvironment::new(false);
	let _translation_state = translate_module(&wasm[..], &mut env).unwrap();

	let shared_builder = settings::builder();
	let shared_flags = settings::Flags::new(shared_builder);
	let isa = isa::lookup(triple!("x86_64-unknown-linux-gnu")).unwrap().finish(shared_flags);
}
