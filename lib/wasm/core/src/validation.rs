mod instructions;
mod modules;
mod types;

use crate::structure::{
	modules::Module,
	types::{FuncType, GlobalType, MemType, ResultType, TableType, ValType},
};
use alloc::vec::Vec;

use modules::validate_module;

pub fn module_validate(module: &Module) -> Result<(), &'static str> {
	info!("validating wasm...");
	validate_module(module)
}

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
