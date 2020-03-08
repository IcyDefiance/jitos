mod binary;
mod structure;

use binary::decode;

pub fn test_wasm() {
	let wasm = include_bytes!("../target/wasm32-unknown-unknown/debug/wasm-test.wasm");
	decode(wasm);
}
