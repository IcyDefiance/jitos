use crate::{
	binary::modules::{module, Custom},
	syntax::{
		instructions::Expr,
		types::{FuncType, GlobalType, MemType, TableType, ValType},
	},
	valid::modules::validate_module,
};
use alloc::vec::Vec;
use nom::{error::ErrorKind, Err};

#[derive(Debug)]
pub struct Module<'a> {
	pub types: Vec<FuncType>,
	pub funcs: Vec<Func>,
	pub tables: Vec<Table>,
	pub mems: Vec<Mem>,
	pub globals: Vec<Global>,
	pub elem: Vec<Elem>,
	pub data: Vec<Data<'a>>,
	pub start: Option<Start>,
	// TODO: imports
	pub exports: Vec<Export<'a>>,
	pub customs: Vec<Custom<'a>>,
}
impl<'a> Module<'a> {
	pub fn decode(bytes: &'a [u8]) -> Result<Self, Err<(&[u8], ErrorKind)>> {
		info!("decoding wasm...");
		module(bytes).map(|r| r.1)
	}

	pub fn validate(&self) -> Result<(), &str> {
		info!("validating wasm...");
		validate_module(self)
	}
}

#[derive(Debug, Default)]
pub struct TypeIdx(pub u32);

#[derive(Debug, Default)]
pub struct FuncIdx(pub u32);

#[derive(Debug, Default)]
pub struct TableIdx(pub u32);

#[derive(Debug, Default)]
pub struct MemIdx(pub u32);

#[derive(Debug, Default)]
pub struct GlobalIdx(pub u32);

#[derive(Debug, Default)]
pub struct LocalIdx(pub u32);

#[derive(Debug, Default)]
pub struct LabelIdx(pub u32);

#[derive(Debug)]
pub struct Func {
	pub typ: TypeIdx,
	pub locals: Vec<ValType>,
	pub body: Expr,
}

#[derive(Debug)]
pub struct Table {
	pub typ: TableType,
}

#[derive(Debug)]
pub struct Mem {
	pub typ: MemType,
}

#[derive(Debug)]
pub struct Global {
	pub typ: GlobalType,
	pub init: Expr,
}

#[derive(Debug)]
pub struct Elem {
	pub table: TableIdx,
	pub offset: Expr,
	pub init: Vec<FuncIdx>,
}

#[derive(Debug)]
pub struct Data<'a> {
	pub data: MemIdx,
	pub offset: Expr,
	pub init: &'a [u8],
}

#[derive(Debug, Default)]
pub struct Start {
	pub func: FuncIdx,
}

#[derive(Debug)]
pub struct Export<'a> {
	pub name: &'a str,
	pub desc: ExportDesc,
}

#[derive(Debug)]
pub enum ExportDesc {
	Func(FuncIdx),
	Table(TableIdx),
	Mem(MemIdx),
	Global(GlobalIdx),
}
