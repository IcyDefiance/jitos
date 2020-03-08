use crate::wasm::{
	structure::{
		instructions::{Expr, Instr},
		types::{ElemType, Mut, ResultType, ValType},
	},
	validation::Context,
};
use alloc::vec::Vec;
use core::{iter::once, slice};

fn validate_instr_seq(
	c: &Context,
	mut stack: Vec<ValType>,
	results: &[ValType],
	instrs: &[Instr],
	konst: bool,
) -> Result<(), &'static str> {
	let pop = |stack: &mut Vec<ValType>, typ: ValType| {
		if stack.pop() == Some(typ) { Ok(()) } else { Err("invalid stack for instr") }
	};
	let pop_any = |stack: &mut Vec<ValType>| stack.pop().ok_or("invalid stack for instr");
	let peek = |stack: &[ValType], typ: ValType| {
		if stack.last() == Some(&typ) { Ok(()) } else { Err("invalid stack for instr") }
	};
	let peek_any = |stack: &Vec<ValType>| stack.last().map(|_| ()).ok_or("invalid stack for instr");

	let testop = |stack: &mut Vec<ValType>, typ: ValType| {
		pop(stack, typ)?;
		stack.push(ValType::I32);
		Ok(())
	};
	let relop = |stack: &mut Vec<ValType>, typ: ValType| {
		pop(stack, typ)?;
		pop(stack, typ)?;
		stack.push(ValType::I32);
		Ok(())
	};
	let unop = |stack: &mut Vec<ValType>, typ: ValType| {
		peek(&stack, typ)?;
		Ok(())
	};
	let binop = |stack: &mut Vec<ValType>, typ: ValType| {
		pop(stack, typ)?;
		peek(&stack, typ)?;
		Ok(())
	};
	let cvtop = |stack: &mut Vec<ValType>, from: ValType, to: ValType| {
		pop(stack, from)?;
		stack.push(to);
		Ok(())
	};

	let mut i = 0;
	while i < instrs.len() {
		let instr = &instrs[i];

		if konst {
			let is_const = match instr {
				Instr::I32Const(_) => true,
				Instr::I64Const(_) => true,
				Instr::GlobalGet(x) => c.globals.get(x.0 as usize).ok_or("undefined global")?.muta == Mut::Const,
				_ => false,
			};
			if !is_const {
				return Err("non-const instr in const expr");
			}
		}

		match instr {
			Instr::Unreachable => return Ok(()),
			Instr::Block(res, expr) | Instr::Loop(res, expr) => {
				let labels = match instr {
					Instr::Block(..) => once(res.clone()).chain(c.labels.iter().cloned()).collect(),
					Instr::Loop(..) => once(ResultType(None)).chain(c.labels.iter().cloned()).collect(),
					_ => unreachable!(),
				};
				let cp = Context { labels, ..c.clone() };
				let results = res.0.as_ref().map(|v| slice::from_ref(v)).unwrap_or(&[]);
				validate_instr_seq(&cp, vec![], results, expr, false)?;
				if let Some(res) = res.0 {
					stack.push(res);
				}
			},
			Instr::Br(lbl) | Instr::BrIf(lbl) => {
				let lbli = lbl.0 as usize;
				if lbli >= c.labels.len() {
					return Err("undefined label");
				}
				if let Instr::BrIf(_) = instr {
					pop(&mut stack, ValType::I32)?;
				}
				if let Some(typ) = c.labels[lbli].0 {
					peek(&stack, typ)?;
				}
				if let Instr::Br(_) = instr {
					return Ok(());
				}
			},
			Instr::BrTable(lbls, last) => {
				let lasti = last.0 as usize;
				if lasti >= c.labels.len() {
					return Err("undefined label");
				}
				for lbl in lbls {
					let lbli = lbl.0 as usize;
					if lbli >= c.labels.len() {
						return Err("undefined label");
					}
					if c.labels[lbl.0 as usize] != c.labels[last.0 as usize] {
						return Err("invalid label in br_table chain");
					}
				}
				pop(&mut stack, ValType::I32)?;
				if let Some(typ) = c.labels[lasti].0 {
					peek(&stack, typ)?;
				}
				return Ok(());
			},
			Instr::Return => {
				if let Some(res) = &c.retur {
					if let Some(typ) = res.0 {
						peek(&stack, typ)?;
					}
				} else {
					return Err("unexpected return");
				}
				return Ok(());
			},
			Instr::Call(x) => {
				let x = x.0 as usize;
				if x >= c.funcs.len() {
					return Err("undefined func");
				}
				let func = &c.funcs[x];
				for &param in func.params.iter().rev() {
					pop(&mut stack, param)?;
				}
				for &result in &func.results {
					stack.push(result);
				}
			},
			Instr::CallIndirect(x) => {
				if c.tables.len() == 0 {
					return Err("undefined table");
				}
				if c.tables[0].et != ElemType::FuncRef {
					return Err("invalid table");
				}
				let x = x.0 as usize;
				if x >= c.types.len() {
					return Err("undefined type");
				}
				let typ = &c.types[x];
				pop(&mut stack, ValType::I32)?;
				for &param in typ.params.iter().rev() {
					pop(&mut stack, param)?;
				}
				for &result in &typ.results {
					stack.push(result);
				}
			},
			Instr::Drop => {
				pop_any(&mut stack)?;
			},
			Instr::Select => {
				pop(&mut stack, ValType::I32)?;
				pop_any(&mut stack)?;
				peek_any(&stack)?;
			},
			Instr::LocalGet(x) => {
				let x = x.0 as usize;
				if x >= c.locals.len() {
					return Err("undefined local");
				}
				stack.push(c.locals[x]);
			},
			Instr::LocalSet(x) => {
				let x = x.0 as usize;
				if x >= c.locals.len() {
					return Err("undefined local");
				}
				pop(&mut stack, c.locals[x])?;
			},
			Instr::LocalTee(x) => {
				let x = x.0 as usize;
				if x >= c.locals.len() {
					return Err("undefined local");
				}
				peek(&mut stack, c.locals[x])?;
			},
			Instr::GlobalGet(x) => {
				let x = x.0 as usize;
				if x >= c.globals.len() {
					return Err("undefined global");
				}
				stack.push(c.globals[x].valtype);
			},
			Instr::GlobalSet(x) => {
				let x = x.0 as usize;
				if x >= c.globals.len() {
					return Err("undefined global");
				}
				let global = &c.globals[x];
				if global.muta != Mut::Var {
					return Err("can't set const global");
				}
				pop(&mut stack, c.globals[x].valtype)?;
			},
			Instr::I32Load(memarg)
			| Instr::I64Load(memarg)
			| Instr::I32Load8S(memarg)
			| Instr::I32Load8U(memarg)
			| Instr::I32Load16U(memarg)
			| Instr::I64Load8U(memarg)
			| Instr::I64Load32U(memarg) => {
				if c.mems.len() == 0 {
					return Err("undefined mem");
				}
				let (bytewidth, typ) = match instr {
					Instr::I32Load(_) => (4, ValType::I32),
					Instr::I64Load(_) => (8, ValType::I64),
					Instr::I32Load8S(_) => (1, ValType::I32),
					Instr::I32Load8U(_) => (1, ValType::I32),
					Instr::I32Load16U(_) => (2, ValType::I32),
					Instr::I64Load8U(_) => (1, ValType::I64),
					Instr::I64Load32U(_) => (4, ValType::I64),
					_ => unreachable!(),
				};
				if 2u32.pow(memarg.align) > bytewidth {
					return Err("invalid align");
				}
				pop(&mut stack, ValType::I32)?;
				stack.push(typ);
			},
			Instr::I32Store(memarg)
			| Instr::I64Store(memarg)
			| Instr::I32Store8(memarg)
			| Instr::I32Store16(memarg) => {
				if c.mems.len() == 0 {
					return Err("undefined mem");
				}
				let (bytewidth, typ) = match instr {
					Instr::I32Store(_) => (4, ValType::I32),
					Instr::I64Store(_) => (8, ValType::I64),
					Instr::I32Store8(_) => (1, ValType::I32),
					Instr::I32Store16(_) => (2, ValType::I32),
					_ => unreachable!(),
				};
				if 2u32.pow(memarg.align) > bytewidth {
					return Err("invalid align");
				}
				pop(&mut stack, typ)?;
				pop(&mut stack, ValType::I32)?;
			},
			Instr::MemoryGrow => {
				if c.mems.len() == 0 {
					return Err("undefined mem");
				}
				peek(&mut stack, ValType::I32)?;
			},
			Instr::I32Const(_) => stack.push(ValType::I32),
			Instr::I64Const(_) => stack.push(ValType::I64),
			Instr::I32Eqz => testop(&mut stack, ValType::I32)?,
			Instr::I32Eq => relop(&mut stack, ValType::I32)?,
			Instr::I32Ne => relop(&mut stack, ValType::I32)?,
			Instr::I32LtS => relop(&mut stack, ValType::I32)?,
			Instr::I32LtU => relop(&mut stack, ValType::I32)?,
			Instr::I32GtS => relop(&mut stack, ValType::I32)?,
			Instr::I32GtU => relop(&mut stack, ValType::I32)?,
			Instr::I32LeS => relop(&mut stack, ValType::I32)?,
			Instr::I32LeU => relop(&mut stack, ValType::I32)?,
			Instr::I32GeS => relop(&mut stack, ValType::I32)?,
			Instr::I32GeU => relop(&mut stack, ValType::I32)?,
			Instr::I64Eqz => testop(&mut stack, ValType::I64)?,
			Instr::I64Eq => relop(&mut stack, ValType::I64)?,
			Instr::I64Ne => relop(&mut stack, ValType::I64)?,
			Instr::I64GtU => relop(&mut stack, ValType::I64)?,
			Instr::I64GeU => relop(&mut stack, ValType::I64)?,
			Instr::I32Clz => unop(&mut stack, ValType::I32)?,
			Instr::I32Ctz => unop(&mut stack, ValType::I32)?,
			Instr::I32Add => relop(&mut stack, ValType::I32)?,
			Instr::I32Sub => relop(&mut stack, ValType::I32)?,
			Instr::I32Mul => relop(&mut stack, ValType::I32)?,
			Instr::I32DivU => relop(&mut stack, ValType::I32)?,
			Instr::I32And => binop(&mut stack, ValType::I32)?,
			Instr::I32Or => binop(&mut stack, ValType::I32)?,
			Instr::I32Xor => binop(&mut stack, ValType::I32)?,
			Instr::I32Shl => binop(&mut stack, ValType::I32)?,
			Instr::I32ShrS => binop(&mut stack, ValType::I32)?,
			Instr::I32ShrU => binop(&mut stack, ValType::I32)?,
			Instr::I32Rotl => binop(&mut stack, ValType::I32)?,
			Instr::I64Add => binop(&mut stack, ValType::I64)?,
			Instr::I64Sub => binop(&mut stack, ValType::I64)?,
			Instr::I64Mul => binop(&mut stack, ValType::I64)?,
			Instr::I64DivU => binop(&mut stack, ValType::I64)?,
			Instr::I64And => binop(&mut stack, ValType::I64)?,
			Instr::I64Or => binop(&mut stack, ValType::I64)?,
			Instr::I64Xor => binop(&mut stack, ValType::I64)?,
			Instr::I64Shl => binop(&mut stack, ValType::I64)?,
			Instr::I64ShrS => binop(&mut stack, ValType::I64)?,
			Instr::I32WrapI64 => cvtop(&mut stack, ValType::I64, ValType::I32)?,
			Instr::I64ExtendI32S => cvtop(&mut stack, ValType::I32, ValType::I64)?,
			Instr::I64ExtendI32U => cvtop(&mut stack, ValType::I32, ValType::I64)?,
		}
		i += 1;
	}

	for &res in results {
		pop(&mut stack, res)?;
	}
	Ok(())
}

pub(super) fn validate_expr(
	c: &Context,
	resulttype: &ResultType,
	expr: &Expr,
	konst: bool,
) -> Result<(), &'static str> {
	let results = resulttype.0.as_ref().map(|v| slice::from_ref(v)).unwrap_or(&[]);
	validate_instr_seq(&c, vec![], results, &expr.instrs, konst)?;
	Ok(())
}
