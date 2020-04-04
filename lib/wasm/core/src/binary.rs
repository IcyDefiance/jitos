// TODO: make private
use crate::{binary::modules::module, syntax::modules::Module};
use nom::{combinator::all_consuming, error::ErrorKind, Err};

pub(crate) mod modules;

mod instructions;
mod types;
mod values;

pub fn decode(bytes: &[u8]) -> Result<Module, Err<(&[u8], ErrorKind)>> {
	all_consuming(module)(bytes).map(|(_, module)| module)
}
