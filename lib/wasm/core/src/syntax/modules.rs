use crate::{
	binary::{decode, modules::Custom},
	exec::{
		modules::instantiate,
		runtime::{Embedder, ExternVal, ModuleInst, Store},
	},
	syntax::{
		instructions::Expr,
		types::{FuncType, GlobalType, MemType, TableType, ValType},
	},
	valid::modules::validate_module,
};
use alloc::vec::Vec;
use core::ops::Add;
use nom::{error::ErrorKind, Err};

#[derive(Debug)]
pub struct Module<'a> {
	types: Vec<FuncType>,
	funcs: Vec<Func>,
	tables: Vec<Table>,
	mems: Vec<Mem>,
	globals: Vec<Global>,
	elem: Vec<Elem>,
	data: Vec<Data<'a>>,
	start: Option<Start>,
	imports: Vec<Import<'a>>,
	exports: Vec<Export<'a>>,
	customs: Vec<Custom<'a>>,
	valid: Option<Result<(), &'static str>>,
}
impl<'a> Module<'a> {
	pub(crate) fn new(
		types: Vec<FuncType>,
		funcs: Vec<Func>,
		tables: Vec<Table>,
		mems: Vec<Mem>,
		globals: Vec<Global>,
		elem: Vec<Elem>,
		data: Vec<Data<'a>>,
		start: Option<Start>,
		imports: Vec<Import<'a>>,
		exports: Vec<Export<'a>>,
		customs: Vec<Custom<'a>>,
	) -> Self {
		Self { types, funcs, tables, mems, globals, elem, data, start, imports, exports, customs, valid: None }
	}

	pub fn types(&self) -> &Vec<FuncType> {
		&self.types
	}

	pub fn funcs(&self) -> &Vec<Func> {
		&self.funcs
	}

	pub fn tables(&self) -> &Vec<Table> {
		&self.tables
	}

	pub fn mems(&self) -> &Vec<Mem> {
		&self.mems
	}

	pub fn globals(&self) -> &Vec<Global> {
		&self.globals
	}

	pub fn elem(&self) -> &Vec<Elem> {
		&self.elem
	}

	pub fn data(&self) -> &Vec<Data<'a>> {
		&self.data
	}

	pub fn start(&self) -> &Option<Start> {
		&self.start
	}

	pub fn imports(&self) -> &Vec<Import<'a>> {
		&self.imports
	}

	pub fn exports(&self) -> &Vec<Export<'a>> {
		&self.exports
	}

	pub fn customs(&self) -> &Vec<Custom<'a>> {
		&self.customs
	}

	pub fn decode(bytes: &'a [u8]) -> Result<Self, Err<(&[u8], ErrorKind)>> {
		let ret = decode(bytes);
		ret
	}

	pub fn validate(&mut self) -> Result<(), &'static str> {
		let ret = if let Some(valid) = self.valid {
			valid
		} else {
			let res = validate_module(self);
			self.valid = Some(res);
			res
		};
		ret
	}

	pub fn instantiate<E: Embedder>(
		&mut self,
		store: &mut Store<E>,
		externs: &[ExternVal<E>],
	) -> Result<ModuleInst<E>, &'static str> {
		let ret = instantiate(store, self, externs);
		ret
	}
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct TypeIdx(pub u32);

#[derive(Clone, Copy, Debug, Default)]
pub struct FuncIdx(pub u32);
impl Add<i32> for FuncIdx {
	type Output = Self;

	fn add(self, other: i32) -> Self {
		FuncIdx(self.0 + other as u32)
	}
}

#[derive(Clone, Copy, Debug, Default)]
pub struct TableIdx(pub u32);

#[derive(Clone, Copy, Debug, Default)]
pub struct MemIdx(pub u32);

#[derive(Clone, Copy, Debug, Default)]
pub struct GlobalIdx(pub u32);

#[derive(Clone, Copy, Debug, Default)]
pub struct LocalIdx(pub u32);

#[derive(Clone, Copy, Debug, Default)]
pub struct LabelIdx(pub u32);

#[derive(Clone, Debug)]
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

#[derive(Debug)]
pub struct Import<'a> {
	pub module: &'a str,
	pub name: &'a str,
	pub desc: ImportDesc,
}

#[derive(Debug)]
pub enum ImportDesc {
	Func(TypeIdx),
	Table(TableType),
	Mem(MemType),
	Global(GlobalType),
}
impl ImportDesc {
	pub fn as_func(&self) -> Option<TypeIdx> {
		match self {
			ImportDesc::Func(x) => Some(*x),
			_ => None,
		}
	}

	pub fn as_table(&self) -> Option<&TableType> {
		match self {
			ImportDesc::Table(x) => Some(x),
			_ => None,
		}
	}

	pub fn as_mem(&self) -> Option<&MemType> {
		match self {
			ImportDesc::Mem(x) => Some(x),
			_ => None,
		}
	}

	pub fn as_global(&self) -> Option<&GlobalType> {
		match self {
			ImportDesc::Global(x) => Some(x),
			_ => None,
		}
	}
}
