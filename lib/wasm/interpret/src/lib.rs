#![no_std]

#[macro_use]
extern crate alloc;
#[allow(unused_imports)]
#[macro_use]
extern crate log;

use alloc::vec::Vec;
use core::{iter::repeat, mem::transmute};
use wasm_core::{
	exec::runtime::{Embedder, Frame, ModuleInst, Store, Val},
	syntax::{
		instructions::{Expr, Instr},
		modules::{Func, Global, Mem, Table},
		types::Mut,
	},
};

const PAGE_SIZE: usize = 65536;

#[derive(Default)]
pub struct Interpreter {
	frames: Vec<Frame>,
	/// u32 is label arity
	labels: Vec<Vec<u32>>,
	stacks: Vec<Vec<Val>>,
}
impl Interpreter {
	fn frame(&mut self) -> &mut Frame {
		self.frames.last_mut().unwrap()
	}

	fn labels(&mut self) -> &mut Vec<u32> {
		self.labels.last_mut().unwrap()
	}

	fn push_val(&mut self, val: Val) {
		self.stacks.last_mut().unwrap().push(val);
	}

	fn pop_val(&mut self) -> Val {
		self.stacks.last_mut().unwrap().pop().unwrap()
	}

	fn push_lbl(&mut self, arity: u32) {
		self.labels.last_mut().unwrap().push(arity);
		self.stacks.push(vec![]);
	}

	fn pop_lbl(&mut self) -> u32 {
		self.stacks.pop().unwrap();
		self.labels.last_mut().unwrap().pop().unwrap()
	}

	fn invoke_func(&mut self, s: &mut Store<Self>, inst: &ModuleInst<Self>, funcaddr: u32) {
		let f = s.funcs[funcaddr as usize].clone();
		let typ = &inst.types[f.typ.0 as usize];
		let locals =
			typ.params.iter().map(|_| self.pop_val()).chain(f.locals.iter().map(|typ| Val::from_type(*typ))).collect();
		self.push_frame(Frame::new(locals));
		self.push_lbl(typ.results.len() as _);
		self.exec(s, inst, &f.body.instrs);
		let results: Vec<_> = typ.results.iter().map(|_| self.pop_val()).collect();
		self.pop_frame();
		for res in results.into_iter().rev() {
			self.push_val(res);
		}
	}

	fn exec(&mut self, s: &mut Store<Self>, inst: &ModuleInst<Self>, instrs: &[Instr]) -> Option<u32> {
		for instr in instrs {
			// debug!("{:?} : {:?}", instr, self.stacks.last().unwrap());
			let lbl = self.exec_instr(s, inst, instr);
			if let Some(lbl) = lbl {
				if lbl > 0 {
					return Some(lbl - 1);
				} else {
					break;
				}
			}
		}
		None
	}

	fn exec_instr(&mut self, s: &mut Store<Self>, inst: &ModuleInst<Self>, instr: &Instr) -> Option<u32> {
		match instr {
			Instr::Unreachable => panic!("unreachable instruction reached"),
			Instr::Block(res, instrs) => {
				self.push_lbl(res.0.is_some() as u32);
				let ret = self.exec(s, inst, instrs);
				if ret.is_some() {
					return ret;
				} else {
					let stack: Vec<_> = self.stacks.pop().unwrap();
					self.pop_lbl();
					self.stacks.last_mut().unwrap().extend(stack);
				}
			},
			Instr::Loop(_, instrs) => loop {
				self.push_lbl(0);
				let ret = self.exec(s, inst, instrs);
				if let Some(ret) = ret {
					if ret == 0 {
						continue;
					} else {
						return Some(ret);
					}
				} else {
					let stack: Vec<_> = self.stacks.pop().unwrap();
					self.pop_lbl();
					self.stacks.last_mut().unwrap().extend(stack);
					break;
				}
			},
			Instr::Br(l) => {
				let i = self.labels().len() - l.0 as usize - 1;
				let n = self.labels()[i];
				let vals: Vec<_> = (0..n).map(|_| self.pop_val()).collect();
				for _ in 0..l.0 + 1 {
					self.pop_lbl();
				}
				for val in vals.into_iter().rev() {
					self.push_val(val);
				}
				return Some(l.0);
			},
			Instr::BrIf(l) => {
				let c = self.pop_val().as_i32();
				if c != 0 {
					return self.exec_instr(s, inst, &Instr::Br(*l));
				}
			},
			// Instr::BrTable(Vec<LabelIdx>, LabelIdx),
			// Instr::Return,
			Instr::Call(x) => self.invoke_func(s, inst, inst.funcaddrs[x.0 as usize]),
			// Instr::CallIndirect(TypeIdx),
			// Instr::Drop,
			Instr::Select => {
				let c = self.pop_val().as_i32();
				let val2 = self.pop_val();
				let val1 = self.pop_val();
				self.push_val(if c != 0 { val1 } else { val2 });
			},
			Instr::LocalGet(x) => {
				let val = self.frame().locals[x.0 as usize];
				self.push_val(val);
			},
			Instr::LocalSet(x) => self.frame().locals[x.0 as usize] = self.pop_val(),
			Instr::LocalTee(x) => {
				let val = self.pop_val();
				self.push_val(val);
				self.push_val(val);
				self.exec_instr(s, inst, &Instr::LocalSet(*x));
			},
			Instr::GlobalGet(x) => self.push_val(s.globals[inst.globaladdrs[x.0 as usize] as usize].0),
			Instr::GlobalSet(x) => s.globals[inst.globaladdrs[x.0 as usize] as usize].0 = self.pop_val(),
			Instr::I32Load(m) => {
				let mem = &s.mems[inst.memaddrs[0] as usize].0;
				let ea = (self.pop_val().as_i32() as u32 + m.offset) as usize;
				let mut bs = [0; 4];
				bs.copy_from_slice(&mem[ea..ea + 4]);
				self.push_val(Val::I32(unsafe { transmute(bs) }));
			},
			// Instr::I64Load(MemArg),
			// Instr::I32Load8S(MemArg),
			// Instr::I32Load8U(MemArg),
			// Instr::I32Load16U(MemArg),
			// Instr::I64Load8U(MemArg),
			// Instr::I64Load32U(MemArg),
			Instr::I32Store(m) => {
				let mem = &mut s.mems[inst.memaddrs[0] as usize].0;
				let c: [u8; 4] = unsafe { transmute(self.pop_val().as_i32()) };
				let ea = (self.pop_val().as_i32() as u32 + m.offset) as usize;
				mem[ea..ea + 4].copy_from_slice(&c);
			},
			// Instr::I64Store(MemArg),
			// Instr::I32Store8(MemArg),
			// Instr::I32Store16(MemArg),
			Instr::MemoryGrow => {
				let mem = &mut s.mems[inst.memaddrs[0] as usize].0;
				let sz = (mem.len() / PAGE_SIZE) as i32;
				let n = self.pop_val().as_i32() as usize;
				mem.resize(mem.len() + n * PAGE_SIZE, 0);
				self.push_val(Val::I32(sz));
			},
			Instr::I32Const(n) => self.push_val(Val::I32(*n)),
			// Instr::I64Const(i64),
			Instr::I32Eqz => {
				let lhs = self.pop_val().as_i32() as u32;
				self.push_val(Val::I32((lhs == 0) as i32));
			},
			Instr::I32Eq => {
				let rhs = self.pop_val().as_i32();
				let lhs = self.pop_val().as_i32();
				self.push_val(Val::I32((lhs == rhs) as i32));
			},
			// Instr::I32Ne,
			// Instr::I32LtS,
			Instr::I32LtU => {
				let rhs = self.pop_val().as_i32() as u32;
				let lhs = self.pop_val().as_i32() as u32;
				self.push_val(Val::I32((lhs < rhs) as i32));
			},
			// Instr::I32GtS,
			Instr::I32GtU => {
				let rhs = self.pop_val().as_i32() as u32;
				let lhs = self.pop_val().as_i32() as u32;
				self.push_val(Val::I32((lhs > rhs) as i32));
			},
			// Instr::I32LeS,
			Instr::I32LeU => {
				let rhs = self.pop_val().as_i32() as u32;
				let lhs = self.pop_val().as_i32() as u32;
				self.push_val(Val::I32((lhs <= rhs) as i32));
			},
			// Instr::I32GeS,
			Instr::I32GeU => {
				let rhs = self.pop_val().as_i32() as u32;
				let lhs = self.pop_val().as_i32() as u32;
				self.push_val(Val::I32((lhs >= rhs) as i32));
			},
			// Instr::I64Eqz,
			// Instr::I64Eq,
			// Instr::I64Ne,
			// Instr::I64GtU,
			// Instr::I64GeU,
			Instr::I32Clz => {
				let lhs = self.pop_val().as_i32();
				self.push_val(Val::I32(lhs.leading_zeros() as _));
			},
			// Instr::I32Ctz,
			Instr::I32Add => {
				let rhs = self.pop_val().as_i32();
				let lhs = self.pop_val().as_i32();
				self.push_val(Val::I32(lhs + rhs));
			},
			Instr::I32Sub => {
				let rhs = self.pop_val().as_i32();
				let lhs = self.pop_val().as_i32();
				self.push_val(Val::I32(lhs - rhs));
			},
			// Instr::I32Mul,
			// Instr::I32DivU,
			Instr::I32And => {
				let rhs = self.pop_val().as_i32();
				let lhs = self.pop_val().as_i32();
				self.push_val(Val::I32(lhs & rhs));
			},
			// Instr::I32Or,
			// Instr::I32Xor,
			Instr::I32Shl => {
				let rhs = self.pop_val().as_i32();
				let lhs = self.pop_val().as_i32();
				self.push_val(Val::I32(lhs << rhs));
			},
			// Instr::I32ShrS,
			Instr::I32ShrU => {
				let rhs = self.pop_val().as_i32() as u32;
				let lhs = self.pop_val().as_i32() as u32;
				self.push_val(Val::I32((lhs >> rhs) as i32));
			},
			// Instr::I32Rotl,
			// Instr::I64Add,
			// Instr::I64Sub,
			// Instr::I64Mul,
			// Instr::I64DivU,
			// Instr::I64And,
			// Instr::I64Or,
			// Instr::I64Xor,
			// Instr::I64Shl,
			// Instr::I64ShrS,
			// Instr::I32WrapI64,
			// Instr::I64ExtendI32S,
			// Instr::I64ExtendI32U,
			_ => unimplemented!("{:?}", instr),
		}
		None
	}
}
impl Embedder for Interpreter {
	type FuncAddr = u32;
	type FuncInst = Func;
	type GlobalAddr = u32;
	type GlobalInst = (Val, Mut);
	type MemAddr = u32;
	type MemInst = (Vec<u8>, Option<u32>);
	type TableAddr = u32;
	type TableInst = (Vec<Option<Self::FuncAddr>>, Option<u32>);

	fn push_frame(&mut self, frame: Frame) {
		self.frames.push(frame);
		self.labels.push(vec![]);
		self.stacks.push(vec![]);
	}

	fn pop_frame(&mut self) -> Frame {
		self.stacks.pop().unwrap();
		self.labels.pop().unwrap();
		self.frames.pop().unwrap()
	}

	fn eval(&mut self, expr: &Expr) -> Val {
		let mut stack = vec![];
		for instr in &expr.instrs {
			match instr {
				&Instr::I32Const(n) => stack.push(Val::I32(n)),
				_ => unimplemented!("{:?}", instr),
			}
		}
		stack.pop().unwrap()
	}

	fn alloc_func(&mut self, s: &mut Store<Self>, func: &Func) -> Self::FuncAddr {
		s.funcs.push(func.clone());
		(s.funcs.len() - 1) as _
	}

	fn alloc_table(&mut self, s: &mut Store<Self>, table: &Table) -> Self::TableAddr {
		s.tables.push((repeat(None).take(table.typ.lim.min as usize).collect(), table.typ.lim.max));
		(s.tables.len() - 1) as _
	}

	fn alloc_mem(&mut self, s: &mut Store<Self>, mem: &Mem) -> Self::MemAddr {
		s.mems.push((repeat(0).take(mem.typ.lim.min as usize * PAGE_SIZE).collect(), mem.typ.lim.max));
		(s.mems.len() - 1) as _
	}

	fn alloc_global(&mut self, s: &mut Store<Self>, global: &Global, val: &Val) -> Self::GlobalAddr {
		s.globals.push((val.clone(), global.typ.muta));
		(s.globals.len() - 1) as _
	}

	fn set_elem(&mut self, s: &mut Store<Self>, table: Self::TableAddr, offset: usize, func: Self::FuncAddr) {
		s.tables[table as usize].0[offset] = Some(func);
	}

	fn set_data(&mut self, s: &mut Store<Self>, mem: Self::MemAddr, offset: i32, data: &[u8]) {
		let offset = offset as usize;
		s.mems[mem as usize].0[offset..offset + data.len()].copy_from_slice(data);
	}

	fn invoke(&mut self, s: &mut Store<Self>, inst: &ModuleInst<Self>, funcaddr: Self::FuncAddr, args: Vec<Val>) {
		let funcinst = &s.funcs[funcaddr as usize];

		let typ = &inst.types[funcinst.typ.0 as usize];
		if args.len() != typ.params.len() {
			panic!("wrong number of args");
		}
		for (arg, &param) in args.iter().zip(&typ.params) {
			if arg.typ() != param {
				panic!("invalid arg");
			}
		}

		self.push_frame(Frame::new(vec![]));
		for arg in args {
			self.push_val(arg);
		}
		self.invoke_func(s, inst, funcaddr);
		self.pop_frame();
	}
}
