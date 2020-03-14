use crate::{
	binary::{
		modules::{funcidx, globalidx, labelidx, localidx, typeidx},
		types::blocktype,
		values::{i32, i64, u32, vec},
	},
	syntax::instructions::{Expr, Instr, MemArg},
};
use nom::{bytes::streaming::tag, multi::many_till, number::streaming::le_u8, IResult};

fn instr(i: &[u8]) -> IResult<&[u8], Instr> {
	let (i, opcode) = le_u8(i)?;
	let ret = match opcode {
		0x00 => (i, Instr::Unreachable),
		// *** CONTROL ***
		0x02..=0x03 => {
			let (i, resulttype) = blocktype(i)?;
			let (i, (instrs, _)) = many_till(instr, tag(&[0x0B]))(i)?;
			let instr = match opcode {
				0x02 => Instr::Block(resulttype, instrs),
				0x03 => Instr::Loop(resulttype, instrs),
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
			let (i, _) = tag(&[0x00])(i)?;
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

fn memarg(i: &[u8]) -> IResult<&[u8], MemArg> {
	let (i, align) = u32(i)?;
	let (i, offset) = u32(i)?;
	Ok((i, MemArg { align, offset }))
}

pub fn expr(i: &[u8]) -> IResult<&[u8], Expr> {
	let (i, (instrs, _)) = many_till(instr, tag(&[0x0B]))(i)?;
	Ok((i, Expr { instrs }))
}
