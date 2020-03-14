// TODO: make private
pub mod instructions;
pub mod modules;
pub mod types;
mod values;

use crate::structure::modules::Module;
use modules::module;
use nom::{error::ErrorKind, Err};

pub fn module_decode(bytes: &[u8]) -> Result<Module, Err<(&[u8], ErrorKind)>> {
	info!("decoding wasm...");
	module(bytes).map(|r| r.1)
}
