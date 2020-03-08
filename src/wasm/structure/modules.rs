use crate::wasm::{
	binary::modules::Custom,
	structure::{
		instructions::Expr,
		types::{FuncType, GlobalType, MemType, TableType, ValType},
	},
};
use alloc::vec::Vec;
use core::fmt;

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
	pub fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
		if self.customs.len() != 0 {
			panic!("display for Custom not implemented yet");
		}

		write!(f, "(module")?;
		for (idx, typ) in self.types.iter().enumerate() {
			write!(f, "\n  ")?;
			typ.write_wat(f, idx)?;
		}
		for (idx, func) in self.funcs.iter().enumerate() {
			write!(f, "\n  ")?;
			func.write_wat(f, idx)?;
		}
		for (idx, table) in self.tables.iter().enumerate() {
			write!(f, "\n  ")?;
			table.write_wat(f, idx)?;
		}
		for (idx, mem) in self.mems.iter().enumerate() {
			write!(f, "\n  ")?;
			mem.write_wat(f, idx)?;
		}
		for (idx, global) in self.globals.iter().enumerate() {
			write!(f, "\n  ")?;
			global.write_wat(f, idx)?;
		}
		for export in &self.exports {
			write!(f, "\n  ")?;
			export.write_wat(f)?;
		}
		for (idx, elem) in self.elem.iter().enumerate() {
			write!(f, "\n  ")?;
			elem.write_wat(f, idx)?;
		}
		write!(f, ")\n")
	}
}

#[derive(Debug, Default)]
pub struct TypeIdx(pub u32);
impl TypeIdx {
	pub fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
		write!(f, "{}", self.0)
	}
}

#[derive(Debug, Default)]
pub struct FuncIdx(pub u32);
impl FuncIdx {
	pub fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
		write!(f, "{}", self.0)
	}
}

#[derive(Debug, Default)]
pub struct TableIdx(pub u32);
impl TableIdx {
	pub fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
		write!(f, "{}", self.0)
	}
}

#[derive(Debug, Default)]
pub struct MemIdx(pub u32);
impl MemIdx {
	pub fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
		write!(f, "{}", self.0)
	}
}

#[derive(Debug, Default)]
pub struct GlobalIdx(pub u32);
impl GlobalIdx {
	pub fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
		write!(f, "{}", self.0)
	}
}

#[derive(Debug, Default)]
pub struct LocalIdx(pub u32);
impl LocalIdx {
	pub fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
		write!(f, "{}", self.0)
	}
}

#[derive(Debug, Default)]
pub struct LabelIdx(pub u32);
impl LabelIdx {
	pub fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
		write!(f, "{}", self.0)
	}
}

#[derive(Debug)]
pub struct Func {
	pub typ: TypeIdx,
	pub locals: Vec<ValType>,
	pub body: Expr,
}
impl Func {
	pub fn write_wat(&self, f: &mut dyn fmt::Write, idx: usize) -> fmt::Result {
		write!(f, "(func (;{};) (type ", idx)?;
		self.typ.write_wat(f)?;
		write!(f, ")")?;
		if self.locals.len() > 0 {
			write!(f, "\n    (local")?;
			for local in &self.locals {
				write!(f, " ")?;
				local.write_wat(f)?;
			}
			write!(f, ")\n    ")?;
		}
		self.body.write_wat(f, 4)?;
		write!(f, ")")?;
		Ok(())
	}
}

#[derive(Debug)]
pub struct Table {
	pub typ: TableType,
}
impl Table {
	pub fn write_wat(&self, f: &mut dyn fmt::Write, idx: usize) -> fmt::Result {
		write!(f, "(table (;{};) ", idx)?;
		self.typ.write_wat(f)?;
		write!(f, ")")
	}
}

#[derive(Debug)]
pub struct Mem {
	pub typ: MemType,
}
impl Mem {
	pub fn write_wat(&self, f: &mut dyn fmt::Write, idx: usize) -> fmt::Result {
		write!(f, "(memory (;{};) ", idx)?;
		self.typ.write_wat(f)?;
		write!(f, ")")
	}
}

#[derive(Debug)]
pub struct Global {
	pub typ: GlobalType,
	pub init: Expr,
}
impl Global {
	pub fn write_wat(&self, f: &mut dyn fmt::Write, idx: usize) -> fmt::Result {
		write!(f, "(global (;{};) (", idx)?;
		self.typ.write_wat(f)?;
		write!(f, " ")?;
		self.init.write_wat(f, 4)?;
		write!(f, "))")
	}
}

#[derive(Debug)]
pub struct Elem {
	pub table: TableIdx,
	pub offset: Expr,
	pub init: Vec<FuncIdx>,
}
impl Elem {
	pub fn write_wat(&self, f: &mut dyn fmt::Write, idx: usize) -> fmt::Result {
		write!(f, "(elem (;{};) (", idx)?;
		self.offset.write_wat(f, 4)?;
		write!(f, ")")?;
		for init in &self.init {
			write!(f, " ")?;
			init.write_wat(f)?;
		}
		write!(f, ")")
	}
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
impl<'a> Export<'a> {
	pub fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
		write!(f, "(export \"{}\" (", self.name)?;
		self.desc.write_wat(f)?;
		write!(f, "))")
	}
}

#[derive(Debug)]
pub enum ExportDesc {
	Func(FuncIdx),
	Table(TableIdx),
	Mem(MemIdx),
	Global(GlobalIdx),
}
impl ExportDesc {
	pub fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
		match self {
			ExportDesc::Func(idx) => {
				write!(f, "func ")?;
				idx.write_wat(f)
			},
			ExportDesc::Table(idx) => {
				write!(f, "table ")?;
				idx.write_wat(f)
			},
			ExportDesc::Mem(idx) => {
				write!(f, "memory ")?;
				idx.write_wat(f)
			},
			ExportDesc::Global(idx) => {
				write!(f, "global ")?;
				idx.write_wat(f)
			},
		}
	}
}
