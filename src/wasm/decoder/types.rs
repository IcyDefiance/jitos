use crate::wasm::decoder::values::{u32, vec};
use alloc::vec::Vec;
use core::{
	convert::TryFrom,
	fmt,
	ops::{Bound, RangeBounds, RangeFrom, RangeInclusive},
};
use nom::{
	branch::alt,
	bytes::streaming::tag,
	combinator::{map, map_res, value},
	error::{make_error, ErrorKind},
	number::streaming::le_u8,
	IResult,
};

#[derive(Clone, Debug)]
pub enum ValType {
	I32 = 0x7F,
	I64 = 0x7E,
	F32 = 0x7D,
	F64 = 0x7C,
}
impl ValType {
	pub fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
		match *self {
			ValType::I32 => write!(f, "i32"),
			ValType::I64 => write!(f, "i64"),
			ValType::F32 => write!(f, "f32"),
			ValType::F64 => write!(f, "f64"),
		}
	}
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
pub fn valtype(i: &[u8]) -> IResult<&[u8], ValType> {
	map_res(le_u8, |x| ValType::try_from(x))(i)
}

pub fn blocktype(i: &[u8]) -> IResult<&[u8], Vec<ValType>> {
	alt((value(vec![], tag(&[0x40])), map(map_res(le_u8, |x| ValType::try_from(x)), |x| vec![x])))(i)
}

#[derive(Debug)]
pub struct FuncType {
	param_types: Vec<ValType>,
	result_types: Vec<ValType>,
}
impl FuncType {
	pub fn write_wat(&self, f: &mut dyn fmt::Write, idx: usize) -> fmt::Result {
		write!(f, "(type (;{};) (func", idx)?;
		if self.param_types.len() > 0 {
			write!(f, " (param")?;
			for param_type in &self.param_types {
				write!(f, " ")?;
				param_type.write_wat(f)?;
			}
			write!(f, ")")?;
		}
		if self.result_types.len() > 0 {
			write!(f, " (result")?;
			for result_type in &self.result_types {
				write!(f, " ")?;
				result_type.write_wat(f)?;
			}
			write!(f, ")")?;
		}
		write!(f, "))")
	}
}

pub fn functype(i: &[u8]) -> IResult<&[u8], FuncType> {
	let (i, _) = tag(&[0x60])(i)?;
	let (i, param_types) = vec(valtype)(i)?;
	let (i, result_types) = vec(valtype)(i)?;
	Ok((i, FuncType { param_types, result_types }))
}

#[derive(Debug)]
pub enum Limits {
	RangeFrom(RangeFrom<u32>),
	RangeInclusive(RangeInclusive<u32>),
}
impl Limits {
	fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
		match self.start_bound() {
			Bound::Included(x) => write!(f, "{}", x)?,
			Bound::Excluded(_) => unreachable!(),
			Bound::Unbounded => (),
		}
		match self.end_bound() {
			Bound::Included(x) => write!(f, " {}", x)?,
			Bound::Excluded(_) => unreachable!(),
			Bound::Unbounded => (),
		}
		Ok(())
	}
}
impl RangeBounds<u32> for Limits {
	fn start_bound(&self) -> Bound<&u32> {
		match self {
			Limits::RangeFrom(range) => range.start_bound(),
			Limits::RangeInclusive(range) => range.start_bound(),
		}
	}

	fn end_bound(&self) -> Bound<&u32> {
		match self {
			Limits::RangeFrom(range) => range.end_bound(),
			Limits::RangeInclusive(range) => range.end_bound(),
		}
	}
}

fn limits(i: &[u8]) -> IResult<&[u8], Limits> {
	let (i, flag) = le_u8(i)?;
	let (i, min) = u32(i)?;
	let ret = match flag {
		0x00 => (i, Limits::RangeFrom(min..)),
		0x01 => {
			let (i, max) = u32(i)?;
			(i, Limits::RangeInclusive(min..=max))
		},
		_ => return Err(nom::Err::Error(make_error(i, ErrorKind::Switch))),
	};
	Ok(ret)
}

#[derive(Debug)]
pub struct MemType {
	pub lim: Limits,
}
impl MemType {
	pub fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
		self.lim.write_wat(f)
	}
}

pub fn memtype(i: &[u8]) -> IResult<&[u8], MemType> {
	let (i, lim) = limits(i)?;
	Ok((i, MemType { lim }))
}

#[derive(Debug)]
pub struct TableType {
	pub lim: Limits,
	pub et: ElemType,
}
impl TableType {
	pub fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
		self.lim.write_wat(f)?;
		write!(f, " ")?;
		self.et.write_wat(f)
	}
}

pub fn tabletype(i: &[u8]) -> IResult<&[u8], TableType> {
	let (i, et) = elemtype(i)?;
	let (i, lim) = limits(i)?;
	Ok((i, TableType { lim, et }))
}

#[derive(Debug)]
pub enum ElemType {
	FuncRef = 0x70,
}
impl ElemType {
	fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
		let text = match *self {
			ElemType::FuncRef => "funcref",
		};
		write!(f, "{}", text)
	}
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

fn elemtype(i: &[u8]) -> IResult<&[u8], ElemType> {
	map_res(le_u8, |x| ElemType::try_from(x))(i)
}

#[derive(Debug)]
pub struct GlobalType {
	muta: Mut,
	valtype: ValType,
}
impl GlobalType {
	pub fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
		if self.muta == Mut::Var {
			write!(f, "(mut ")?;
		}
		self.valtype.write_wat(f)?;
		if self.muta == Mut::Var {
			write!(f, ")")?;
		}
		Ok(())
	}
}

pub fn globaltype(i: &[u8]) -> IResult<&[u8], GlobalType> {
	let (i, valtype) = valtype(i)?;
	let (i, muta) = muta(i)?;
	Ok((i, GlobalType { valtype, muta }))
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

fn muta(i: &[u8]) -> IResult<&[u8], Mut> {
	map_res(le_u8, |x| Mut::try_from(x))(i)
}
