use crate::wasm::structure::types::{FuncType, GlobalType, MemType, ResultType, TableType, ValType};
use alloc::vec::Vec;

mod instructions;
mod modules;
mod types;

pub use modules::validate_module as validate;

#[derive(Clone, Debug, Default)]
struct Context<'a> {
	types: Vec<&'a FuncType>,
	funcs: Vec<&'a FuncType>,
	tables: Vec<&'a TableType>,
	mems: Vec<&'a MemType>,
	globals: Vec<&'a GlobalType>,
	locals: Vec<ValType>,
	labels: Vec<ResultType>,
	retur: Option<ResultType>,
}
