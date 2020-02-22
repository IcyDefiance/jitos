mod jitos_env;

use cranelift_codegen::isa::{CallConv, TargetFrontendConfig};
use cranelift_wasm::{translate_module, ReturnMode};
use jitos_env::JitosEnvironment;
use target_lexicon::PointerWidth;

pub fn test_wasm() {
	let wasm = include_bytes!("../target/wasm32-unknown-unknown/debug/wasm-test.wasm");
	let config = TargetFrontendConfig { default_call_conv: CallConv::Fast, pointer_width: PointerWidth::U64 };
	let _translation_state =
		translate_module(&wasm[..], &mut JitosEnvironment::new(config, ReturnMode::NormalReturns, true)).unwrap();
}
