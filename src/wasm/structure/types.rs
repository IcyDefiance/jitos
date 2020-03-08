use alloc::vec::Vec;
use core::{
	convert::TryFrom,
	ops::{Bound, RangeBounds},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ValType {
	I32 = 0x7F,
	I64 = 0x7E,
	F32 = 0x7D,
	F64 = 0x7C,
}
impl TryFrom<u8> for ValType {
	type Error = &'static str;

	fn try_from(value: u8) -> Result<Self, Self::Error> {
		match value {
			0x7F => Ok(ValType::I32),
			0x7E => Ok(ValType::I64),
			0x7D => Ok(ValType::F32),
			0x7C => Ok(ValType::F64),
			_ => Err("invalid ValType"),
		}
	}
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ResultType(pub Option<ValType>);

#[derive(Debug)]
pub struct FuncType {
	pub params: Vec<ValType>,
	pub results: Vec<ValType>,
}

#[derive(Debug)]
pub struct Limits {
	pub min: u32,
	pub max: Option<u32>,
}
impl RangeBounds<u32> for Limits {
	fn start_bound(&self) -> Bound<&u32> {
		Bound::Included(&self.min)
	}

	fn end_bound(&self) -> Bound<&u32> {
		match &self.max {
			Some(max) => Bound::Included(max),
			None => Bound::Unbounded,
		}
	}
}

#[derive(Debug)]
pub struct MemType {
	pub lim: Limits,
}

#[derive(Debug)]
pub struct TableType {
	pub lim: Limits,
	pub et: ElemType,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ElemType {
	FuncRef = 0x70,
}
impl TryFrom<u8> for ElemType {
	type Error = &'static str;

	fn try_from(value: u8) -> Result<Self, Self::Error> {
		match value {
			0x70 => Ok(ElemType::FuncRef),
			_ => Err("invalid ElemType"),
		}
	}
}

#[derive(Debug)]
pub struct GlobalType {
	pub muta: Mut,
	pub valtype: ValType,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Mut {
	Const = 0x00,
	Var = 0x01,
}
impl TryFrom<u8> for Mut {
	type Error = &'static str;

	fn try_from(value: u8) -> Result<Self, Self::Error> {
		match value {
			0x00 => Ok(Mut::Const),
			0x01 => Ok(Mut::Var),
			_ => Err("invalid Mut"),
		}
	}
}
