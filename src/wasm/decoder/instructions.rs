use crate::wasm::decoder::{
	modules::{funcidx, globalidx, labelidx, localidx, typeidx, FuncIdx, GlobalIdx, LabelIdx, LocalIdx, TypeIdx},
	types::{blocktype, ValType},
	values::{i32, i64, u32, vec},
};
use alloc::vec::Vec;
use core::fmt;
use nom::{bytes::streaming::tag, multi::many_till, number::streaming::le_u8, IResult};

#[derive(Debug)]
pub enum Instr {
	Unreachable,
	Block(Block),
	Loop(Block),
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
impl Instr {
	pub fn write_wat(&self, f: &mut dyn fmt::Write, indent: usize) -> fmt::Result {
		match self {
			Instr::Unreachable => write!(f, "unreachable")?,
			Instr::Block(block) | Instr::Loop(block) => {
				let op = match self {
					Instr::Block(_) => "block",
					Instr::Loop(_) => "loop",
					_ => unreachable!(),
				};
				write!(f, "{} ", op)?;
				let subindent = indent + 2;
				for instr in &block.body {
					write!(f, "\n{:indent$}", "", indent = subindent)?;
					instr.write_wat(f, subindent)?;
				}
				write!(f, "\n{:indent$}end", "", indent = indent)?;
			},
			Instr::Br(l) | Instr::BrIf(l) => {
				let op = match self {
					Instr::Br(_) => "br",
					Instr::BrIf(_) => "br_if",
					_ => unreachable!(),
				};
				write!(f, "{} ", op)?;
				l.write_wat(f)?;
			},
			Instr::BrTable(ls, ln) => {
				write!(f, "br_table")?;
				for l in ls {
					write!(f, " ")?;
					l.write_wat(f)?;
				}
				write!(f, " ")?;
				ln.write_wat(f)?;
			},
			Instr::Return => write!(f, "return")?,
			Instr::Call(x) => {
				write!(f, "call ")?;
				x.write_wat(f)?;
			},
			Instr::CallIndirect(x) => {
				write!(f, "call_indirect ")?;
				x.write_wat(f)?;
			},
			Instr::Drop => write!(f, "drop")?,
			Instr::Select => write!(f, "select")?,
			Instr::LocalGet(x) | Instr::LocalSet(x) | Instr::LocalTee(x) => {
				let op = match self {
					Instr::LocalGet(_) => "local.get",
					Instr::LocalSet(_) => "local.set",
					Instr::LocalTee(_) => "local.tee",
					_ => unreachable!(),
				};
				write!(f, "{} ", op)?;
				x.write_wat(f)?;
			},
			Instr::GlobalGet(x) | Instr::GlobalSet(x) => {
				let op = match self {
					Instr::GlobalGet(_) => "global.get",
					Instr::GlobalSet(_) => "global.set",
					_ => unreachable!(),
				};
				write!(f, "{} ", op)?;
				x.write_wat(f)?;
			},
			Instr::I32Load(m)
			| Instr::I64Load(m)
			| Instr::I32Load8S(m)
			| Instr::I32Load8U(m)
			| Instr::I32Load16U(m)
			| Instr::I64Load8U(m)
			| Instr::I64Load32U(m)
			| Instr::I32Store(m)
			| Instr::I64Store(m)
			| Instr::I32Store8(m)
			| Instr::I32Store16(m) => {
				let op = match self {
					Instr::I32Load(_) => "i32.load",
					Instr::I64Load(_) => "i64.load",
					Instr::I32Load8S(_) => "i32.load8_s",
					Instr::I32Load8U(_) => "i32.load8_u",
					Instr::I32Load16U(_) => "i32.load16_u",
					Instr::I64Load8U(_) => "i64.load8_u",
					Instr::I64Load32U(_) => "i64.load32_u",
					Instr::I32Store(_) => "i32.store",
					Instr::I64Store(_) => "i64.store",
					Instr::I32Store8(_) => "i32.store8",
					Instr::I32Store16(_) => "i32.store16",
					_ => unreachable!(),
				};
				write!(f, "{} ", op)?;
				m.write_wat(f)?;
			},
			Instr::MemoryGrow => write!(f, "memory.grow")?,
			Instr::I32Const(n) => write!(f, "i32.const {}", n)?,
			Instr::I64Const(n) => write!(f, "i64.const {}", n)?,
			Instr::I32Eqz => write!(f, "i32.eqz")?,
			Instr::I32Eq => write!(f, "i32.eq")?,
			Instr::I32Ne => write!(f, "i32.ne")?,
			Instr::I32LtS => write!(f, "i32.lt_s")?,
			Instr::I32LtU => write!(f, "i32.lt_u")?,
			Instr::I32LeS => write!(f, "i32.le_s")?,
			Instr::I32LeU => write!(f, "i32.le_u")?,
			Instr::I32GtS => write!(f, "i32.gt_s")?,
			Instr::I32GtU => write!(f, "i32.gt_u")?,
			Instr::I32GeS => write!(f, "i32.ge_s")?,
			Instr::I32GeU => write!(f, "i32.ge_u")?,
			Instr::I64Eqz => write!(f, "i64.eqz")?,
			Instr::I64Eq => write!(f, "i64.eq")?,
			Instr::I64Ne => write!(f, "i64.ne")?,
			Instr::I64GtU => write!(f, "i64.gt_u")?,
			Instr::I64GeU => write!(f, "i64.ge_u")?,
			Instr::I32Clz => write!(f, "i32.clz")?,
			Instr::I32Ctz => write!(f, "i32.ctz")?,
			Instr::I32Add => write!(f, "i32.add")?,
			Instr::I32Sub => write!(f, "i32.sub")?,
			Instr::I32Mul => write!(f, "i32.mul")?,
			Instr::I32DivU => write!(f, "i32.div_u")?,
			Instr::I32And => write!(f, "i32.and")?,
			Instr::I32Or => write!(f, "i32.or")?,
			Instr::I32Xor => write!(f, "i32.xor")?,
			Instr::I32Shl => write!(f, "i32.shl")?,
			Instr::I32ShrS => write!(f, "i32.shr_s")?,
			Instr::I32ShrU => write!(f, "i32.shr_u")?,
			Instr::I32Rotl => write!(f, "i32.rotl")?,
			Instr::I64Add => write!(f, "i64.add")?,
			Instr::I64Sub => write!(f, "i64.sub")?,
			Instr::I64Mul => write!(f, "i64.mul")?,
			Instr::I64DivU => write!(f, "i64.div_u")?,
			Instr::I64And => write!(f, "i64.and")?,
			Instr::I64Or => write!(f, "i64.or")?,
			Instr::I64Xor => write!(f, "i64.xor")?,
			Instr::I64Shl => write!(f, "i64.shl")?,
			Instr::I64ShrS => write!(f, "i64.shr_s")?,
			Instr::I32WrapI64 => write!(f, "i32.wrap_i64")?,
			Instr::I64ExtendI32S => write!(f, "i64.extend_i32_s")?,
			Instr::I64ExtendI32U => write!(f, "i64.extend_i32_u")?,
		}
		Ok(())
	}
}

#[derive(Debug)]
pub struct Block {
	typ: Vec<ValType>,
	body: Vec<Instr>,
}

fn instr(i: &[u8]) -> IResult<&[u8], Instr> {
	let (i, opcode) = le_u8(i)?;
	let ret = match opcode {
		0x00 => (i, Instr::Unreachable),
		// *** CONTROL ***
		0x02..=0x03 => {
			let (i, typ) = blocktype(i)?;
			let (i, (body, _)) = many_till(instr, tag(&[0x0B]))(i)?;
			let instr = match opcode {
				0x02 => Instr::Block(Block { typ, body }),
				0x03 => Instr::Loop(Block { typ, body }),
				_ => unreachable!(),
			};
			(i, instr)
		},
		0x0C..=0x0D => {
			let (i, l) = labelidx(i)?;
			let instr = match opcode {
				0x0C => Instr::Br(l),
				0x0D => Instr::BrIf(l),
				_ => unreachable!(),
			};
			(i, instr)
		},
		0x0E => {
			let (i, ls) = vec(labelidx)(i)?;
			let (i, ln) = labelidx(i)?;
			(i, Instr::BrTable(ls, ln))
		},
		0x0F => (i, Instr::Return),
		0x10 => {
			let (i, x) = funcidx(i)?;
			(i, Instr::Call(x))
		},
		0x11 => {
			let (i, x) = typeidx(i)?;
			(i, Instr::CallIndirect(x))
		},
		// *** PARAMETRIC ***
		0x1A => (i, Instr::Drop),
		0x1B => (i, Instr::Select),
		// *** VARIABLE ***
		0x20..=0x22 => {
			let (i, x) = localidx(i)?;
			let instr = match opcode {
				0x20 => Instr::LocalGet(x),
				0x21 => Instr::LocalSet(x),
				0x22 => Instr::LocalTee(x),
				_ => unreachable!(),
			};
			(i, instr)
		},
		0x23..=0x24 => {
			let (i, x) = globalidx(i)?;
			let instr = match opcode {
				0x23 => Instr::GlobalGet(x),
				0x24 => Instr::GlobalSet(x),
				_ => unreachable!(),
			};
			(i, instr)
		},
		// *** MEMORY ***
		0x28..=0x29 | 0x2C..=0x2D | 0x2F | 0x31 | 0x35..=0x37 | 0x3A..=0x3B => {
			let (i, m) = memarg(i)?;
			let instr = match opcode {
				0x28 => Instr::I32Load(m),
				0x29 => Instr::I64Load(m),
				0x2C => Instr::I32Load8S(m),
				0x2D => Instr::I32Load8U(m),
				0x2F => Instr::I32Load16U(m),
				0x31 => Instr::I64Load8U(m),
				0x35 => Instr::I64Load32U(m),
				0x36 => Instr::I32Store(m),
				0x37 => Instr::I64Store(m),
				0x3A => Instr::I32Store8(m),
				0x3B => Instr::I32Store16(m),
				_ => unreachable!(),
			};
			(i, instr)
		},
		0x40 => (i, Instr::MemoryGrow),
		// *** NUMERIC ***
		0x41 => {
			let (i, n) = i32(i)?;
			(i, Instr::I32Const(n))
		},
		0x42 => {
			let (i, n) = i64(i)?;
			(i, Instr::I64Const(n))
		},
		0x45 => (i, Instr::I32Eqz),
		0x46 => (i, Instr::I32Eq),
		0x47 => (i, Instr::I32Ne),
		0x48 => (i, Instr::I32LtS),
		0x49 => (i, Instr::I32LtU),
		0x4A => (i, Instr::I32GtS),
		0x4B => (i, Instr::I32GtU),
		0x4C => (i, Instr::I32LeS),
		0x4D => (i, Instr::I32LeU),
		0x4E => (i, Instr::I32GeS),
		0x4F => (i, Instr::I32GeU),
		0x50 => (i, Instr::I64Eqz),
		0x51 => (i, Instr::I64Eq),
		0x52 => (i, Instr::I64Ne),
		0x56 => (i, Instr::I64GtU),
		0x5A => (i, Instr::I64GeU),
		0x67 => (i, Instr::I32Clz),
		0x68 => (i, Instr::I32Ctz),
		0x6A => (i, Instr::I32Add),
		0x6B => (i, Instr::I32Sub),
		0x6C => (i, Instr::I32Mul),
		0x6E => (i, Instr::I32DivU),
		0x71 => (i, Instr::I32And),
		0x72 => (i, Instr::I32Or),
		0x73 => (i, Instr::I32Xor),
		0x74 => (i, Instr::I32Shl),
		0x75 => (i, Instr::I32ShrS),
		0x76 => (i, Instr::I32ShrU),
		0x77 => (i, Instr::I32Rotl),
		0x7C => (i, Instr::I64Add),
		0x7D => (i, Instr::I64Sub),
		0x7E => (i, Instr::I64Mul),
		0x80 => (i, Instr::I64DivU),
		0x83 => (i, Instr::I64And),
		0x84 => (i, Instr::I64Or),
		0x85 => (i, Instr::I64Xor),
		0x86 => (i, Instr::I64Shl),
		0x87 => (i, Instr::I64ShrS),
		0xA7 => (i, Instr::I32WrapI64),
		0xAC => (i, Instr::I64ExtendI32S),
		0xAD => (i, Instr::I64ExtendI32U),
		_ => unimplemented!("{:#x}", opcode),
	};
	Ok(ret)
}

#[derive(Debug)]
pub struct MemArg {
	pub align: u32,
	pub offset: u32,
}
impl MemArg {
	pub fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
		write!(f, "align={} offset={}", self.align, self.offset)
	}
}
fn memarg(i: &[u8]) -> IResult<&[u8], MemArg> {
	let (i, align) = u32(i)?;
	let (i, offset) = u32(i)?;
	Ok((i, MemArg { align, offset }))
}

#[derive(Debug)]
pub struct Expr {
	pub instrs: Vec<Instr>,
}
impl Expr {
	pub fn write_wat(&self, f: &mut dyn fmt::Write, indent: usize) -> fmt::Result {
		if self.instrs.len() > 0 {
			self.instrs[0].write_wat(f, indent)?;
			for instr in self.instrs.iter().skip(1) {
				write!(f, "\n{:indent$}", "", indent = indent)?;
				instr.write_wat(f, indent)?;
			}
		}
		Ok(())
	}
}

pub fn expr(i: &[u8]) -> IResult<&[u8], Expr> {
	let (i, (instrs, _)) = many_till(instr, tag(&[0x0B]))(i)?;
	Ok((i, Expr { instrs }))
}
