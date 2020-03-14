pub mod modules;

mod instructions;
mod types;

use crate::syntax::types::{FuncType, GlobalType, MemType, ResultType, TableType, ValType};
use alloc::vec::Vec;

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
