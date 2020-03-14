use crate::syntax::{
	modules::Func,
	types::{FuncType, Mut},
};
use alloc::{string::String, vec::Vec};

enum Val {
	I32Const(i32),
	I64Const(i64),
	F32Const(f32),
	F64Const(f64),
}

pub struct Store {
	funcs: Vec<FuncInst>,
	tables: Vec<TableInst>,
	mems: Vec<MemInst>,
	globals: Vec<GlobalInst>,
}
impl Store {
	pub fn init() -> Self {
		Self { funcs: vec![], tables: vec![], mems: vec![], globals: vec![] }
	}
}

struct FuncAddr(u32);
struct TableAddr(u32);
struct MemAddr(u32);
struct GlobalAddr(u32);

struct ModuleInst {
	types: Vec<FuncType>,
	funcaddrs: Vec<FuncAddr>,
	tableaddrs: Vec<TableAddr>,
	memaddrs: Vec<MemAddr>,
	globaladdrs: Vec<GlobalAddr>,
	exports: Vec<ExportInst>,
}

struct FuncInst {
	typ: FuncType,
	module: ModuleInst,
	code: Func,
}

struct TableInst {
	elem: Vec<FuncElem>,
	max: Option<u32>,
}

struct FuncElem(Option<FuncAddr>);

struct MemInst {
	data: Vec<u8>,
	max: Option<u32>,
}

struct GlobalInst {
	value: Val,
	muta: Mut,
}

struct ExportInst {
	name: String,
	value: ExternVal,
}

enum ExternVal {
	Func(FuncAddr),
	Table(TableAddr),
	Mem(MemAddr),
	Global(GlobalAddr),
}
