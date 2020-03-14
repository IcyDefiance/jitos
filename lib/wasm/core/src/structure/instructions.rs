use crate::structure::{
	modules::{FuncIdx, GlobalIdx, LabelIdx, LocalIdx, TypeIdx},
	types::{ResultType, ValType},
};
use alloc::vec::Vec;

#[derive(Debug)]
pub enum Instr {
	Unreachable,
	Block(ResultType, Vec<Instr>),
	Loop(ResultType, Vec<Instr>),
	Br(LabelIdx),
	BrIf(LabelIdx),
	BrTable(Vec<LabelIdx>, LabelIdx),
	Return,
	Call(FuncIdx),
	CallIndirect(TypeIdx),
	Drop,
	Select,
	LocalGet(LocalIdx),
	LocalSet(LocalIdx),
	LocalTee(LocalIdx),
	GlobalGet(GlobalIdx),
	GlobalSet(GlobalIdx),
	I32Load(MemArg),
	I64Load(MemArg),
	I32Load8S(MemArg),
	I32Load8U(MemArg),
	I32Load16U(MemArg),
	I64Load8U(MemArg),
	I64Load32U(MemArg),
	I32Store(MemArg),
	I64Store(MemArg),
	I32Store8(MemArg),
	I32Store16(MemArg),
	MemoryGrow,
	I32Const(i32),
	I64Const(i64),
	I32Eqz,
	I32Eq,
	I32Ne,
	I32LtS,
	I32LtU,
	I32GtS,
	I32GtU,
	I32LeS,
	I32LeU,
	I32GeS,
	I32GeU,
	I64Eqz,
	I64Eq,
	I64Ne,
	I64GtU,
	I64GeU,
	I32Clz,
	I32Ctz,
	I32Add,
	I32Sub,
	I32Mul,
	I32DivU,
	I32And,
	I32Or,
	I32Xor,
	I32Shl,
	I32ShrS,
	I32ShrU,
	I32Rotl,
	I64Add,
	I64Sub,
	I64Mul,
	I64DivU,
	I64And,
	I64Or,
	I64Xor,
	I64Shl,
	I64ShrS,
	I32WrapI64,
	I64ExtendI32S,
	I64ExtendI32U,
}

#[derive(Debug)]
pub struct MemArg {
	pub align: u32,
	pub offset: u32,
}

#[derive(Debug)]
pub struct Expr {
	pub instrs: Vec<Instr>,
}

#[derive(Debug)]
pub struct Block {
	pub typ: Vec<ValType>,
	pub body: Vec<Instr>,
}
