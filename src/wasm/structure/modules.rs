use crate::wasm::{
	binary::modules::Custom,
	structure::{
		instructions::Expr,
		types::{FuncType, GlobalType, MemType, TableType, ValType},
	},
};
use alloc::vec::Vec;

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
