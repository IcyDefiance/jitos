// TODO: make private
use crate::{binary::modules::module, syntax::modules::Module};
use nom::{error::ErrorKind, Err};

pub(crate) mod modules;

mod instructions;
mod types;
mod values;

pub fn decode(bytes: &[u8]) -> Result<Module, Err<(&[u8], ErrorKind)>> {
	module(bytes).map(|(rem, module)| {
		assert_eq!(rem.len(), 0);
		module
	})
}
