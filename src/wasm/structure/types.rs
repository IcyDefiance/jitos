use alloc::vec::Vec;
use core::{
	convert::TryFrom,
	fmt,
	ops::{Bound, RangeBounds},
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

#[derive(Clone, Debug)]
pub struct ResultType(pub Option<ValType>);

#[derive(Debug)]
pub struct FuncType {
	pub params: Vec<ValType>,
	pub results: Vec<ValType>,
}
impl FuncType {
	pub fn write_wat(&self, f: &mut dyn fmt::Write, idx: usize) -> fmt::Result {
		write!(f, "(type (;{};) (func", idx)?;
		if self.params.len() > 0 {
			write!(f, " (param")?;
			for param_type in &self.params {
				write!(f, " ")?;
				param_type.write_wat(f)?;
			}
			write!(f, ")")?;
		}
		if self.results.len() > 0 {
			write!(f, " (result")?;
			for result_type in &self.results {
				write!(f, " ")?;
				result_type.write_wat(f)?;
			}
			write!(f, ")")?;
		}
		write!(f, "))")
	}
}

#[derive(Debug)]
pub struct Limits {
	pub min: u32,
	pub max: Option<u32>,
}
impl Limits {
	pub fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
		write!(f, "{}", self.min)?;
		if let Some(max) = self.max {
			write!(f, "{}", max)?;
		}
		Ok(())
	}
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
impl MemType {
	pub fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
		self.lim.write_wat(f)
	}
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

#[derive(Debug)]
pub enum ElemType {
	FuncRef = 0x70,
}
impl ElemType {
	pub fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
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

#[derive(Debug)]
pub struct GlobalType {
	pub muta: Mut,
	pub valtype: ValType,
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
