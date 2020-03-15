use crate::syntax::{
	instructions::Expr,
	modules::{Func, Global, Mem, Table},
	types::{FuncType, ValType},
};
use alloc::{string::String, vec::Vec};
use hashbrown::HashMap;

#[derive(Clone, Copy, Debug)]
pub enum Val {
	I32(i32),
	I64(i64),
	F32(f32),
	F64(f64),
}
impl Val {
	pub fn from_type(typ: ValType) -> Self {
		match typ {
			ValType::I32 => Val::I32(0),
			ValType::I64 => Val::I64(0),
			ValType::F32 => Val::F32(0.0),
			ValType::F64 => Val::F64(0.0),
		}
	}

	pub fn as_i32(self) -> i32 {
		match self {
			Val::I32(x) => x,
			_ => panic!("not an i32"),
		}
	}

	pub fn typ(self) -> ValType {
		match self {
			Val::I32(_) => ValType::I32,
			Val::I64(_) => ValType::I64,
			Val::F32(_) => ValType::F32,
			Val::F64(_) => ValType::F64,
		}
	}
}

pub trait Embedder: Default {
	type FuncAddr: Clone;
	type FuncInst;
	type GlobalAddr: Clone;
	type GlobalInst;
	type MemAddr: Clone;
	type MemInst;
	type TableAddr: Clone;
	type TableInst;

	fn push_frame(&mut self, frame: Frame);
	fn pop_frame(&mut self) -> Frame;
	fn eval(&mut self, expr: &Expr) -> Val;
	fn alloc_func(&mut self, s: &mut Store<Self>, func: &Func) -> Self::FuncAddr;
	fn alloc_table(&mut self, s: &mut Store<Self>, table: &Table) -> Self::TableAddr;
	fn alloc_mem(&mut self, s: &mut Store<Self>, mem: &Mem) -> Self::MemAddr;
	fn alloc_global(&mut self, s: &mut Store<Self>, global: &Global, val: &Val) -> Self::GlobalAddr;
	fn set_elem(&mut self, s: &mut Store<Self>, table: Self::TableAddr, offset: usize, func: Self::FuncAddr);
	fn set_data(&mut self, s: &mut Store<Self>, mem: Self::MemAddr, offset: i32, data: &[u8]);
	fn invoke(&mut self, s: &mut Store<Self>, inst: &ModuleInst<Self>, funcaddr: Self::FuncAddr, args: Vec<Val>);
}

pub struct Store<E: Embedder> {
	pub funcs: Vec<E::FuncInst>,
	pub tables: Vec<E::TableInst>,
	pub mems: Vec<E::MemInst>,
	pub globals: Vec<E::GlobalInst>,
}
impl<E: Embedder> Store<E> {
	pub fn init() -> Self {
		Self { funcs: vec![], tables: vec![], mems: vec![], globals: vec![] }
	}

	pub fn invoke(&mut self, inst: &ModuleInst<E>, funcaddr: E::FuncAddr, args: Vec<Val>) {
		E::default().invoke(self, inst, funcaddr, args);
	}
}

#[derive(Default)]
pub struct ModuleInst<E: Embedder> {
	pub types: Vec<FuncType>,
	pub funcaddrs: Vec<E::FuncAddr>,
	pub tableaddrs: Vec<E::TableAddr>,
	pub memaddrs: Vec<E::MemAddr>,
	pub globaladdrs: Vec<E::GlobalAddr>,
	pub exports: HashMap<String, ExternVal<E>>,
}

#[derive(Clone)]
pub enum ExternVal<E: Embedder> {
	Func(E::FuncAddr),
	Table(E::TableAddr),
	Mem(E::MemAddr),
	Global(E::GlobalAddr),
}
impl<E: Embedder> ExternVal<E> {
	pub fn as_func(&self) -> E::FuncAddr {
		match self {
			ExternVal::Func(x) => x.clone(),
			_ => panic!("not a func"),
		}
	}
}

pub struct Frame {
	pub locals: Vec<Val>,
}
impl Frame {
	pub fn new(locals: Vec<Val>) -> Self {
		Self { locals }
	}
}
