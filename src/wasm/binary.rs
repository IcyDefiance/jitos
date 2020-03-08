// TODO: make private
pub mod instructions;
pub mod modules;
pub mod types;
mod values;

use crate::wasm::structure::modules::Module;
use modules::module;

pub fn decode(bytes: &[u8]) -> Module {
	module(bytes).unwrap().1
}
