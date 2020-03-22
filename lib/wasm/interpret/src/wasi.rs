use crate::Interpreter;
use alloc::vec::Vec;
use hashbrown::HashMap;
use jitos_wasi::{environ_get, environ_sizes_get, fd_prestat_dir_name, fd_prestat_get, fd_write, proc_exit};
use lazy_static::lazy_static;
use wasm_core::{
	exec::runtime::{ModuleInst, Store, Val},
	syntax::types::ValType,
};

lazy_static! {
	pub static ref WASI: HashMap<&'static str, (unsafe fn(&mut Store<Interpreter>, &ModuleInst<Interpreter>, &mut Vec<Val>), WasiType)> = {
		let mut ret: HashMap<
			&'static str,
			(unsafe fn(&mut Store<Interpreter>, &ModuleInst<Interpreter>, &mut Vec<Val>), WasiType),
		> = HashMap::new();
		ret.insert("environ_get", (adapt_environ_get, ENVIRON_GET_TYPE));
		ret.insert("environ_sizes_get", (adapt_environ_sizes_get, ENVIRON_SIZES_GET_TYPE));
		ret.insert("fd_prestat_get", (adapt_fd_prestat_get, FD_PRESTAT_GET_TYPE));
		ret.insert("fd_prestat_dir_name", (adapt_fd_prestat_dir_name, FD_PRESTAT_GET_DIR_NAME_TYPE));
		ret.insert("fd_write", (adapt_fd_write, FD_WRITE_TYPE));
		ret.insert("proc_exit", (adapt_proc_exit, PROC_EXIT_TYPE));
		ret
	};
}

type WasiType = (&'static [ValType], &'static [ValType]);

const ENVIRON_GET_TYPE: WasiType = (&[ValType::I32, ValType::I32], &[ValType::I32]);
unsafe fn adapt_environ_get(s: &mut Store<Interpreter>, inst: &ModuleInst<Interpreter>, stack: &mut Vec<Val>) {
	let mem = &mut s.mems[inst.memaddrs[0] as usize].0;

	let p1 = stack.pop().unwrap().as_i32();
	let p2 = stack.pop().unwrap().as_i32();
	let p1 = mem[p1 as usize..].as_mut_ptr() as _;
	let p2 = mem[p2 as usize..].as_mut_ptr() as _;
	let ret = environ_get(p1, p2);
	stack.push(Val::I32(ret as _));
}

const ENVIRON_SIZES_GET_TYPE: WasiType = (&[ValType::I32, ValType::I32], &[ValType::I32]);
unsafe fn adapt_environ_sizes_get(s: &mut Store<Interpreter>, inst: &ModuleInst<Interpreter>, stack: &mut Vec<Val>) {
	let mem = &mut s.mems[inst.memaddrs[0] as usize].0;

	let p1 = stack.pop().unwrap().as_i32();
	let p2 = stack.pop().unwrap().as_i32();

	let p1 = mem[p1 as usize..].as_mut_ptr() as _;
	let p2 = mem[p2 as usize..].as_mut_ptr() as _;
	let ret = environ_sizes_get(p1, p2);
	stack.push(Val::I32(ret as _));
}

const FD_PRESTAT_GET_TYPE: WasiType = (&[ValType::I32, ValType::I32], &[ValType::I32]);
unsafe fn adapt_fd_prestat_get(s: &mut Store<Interpreter>, inst: &ModuleInst<Interpreter>, stack: &mut Vec<Val>) {
	let mem = &mut s.mems[inst.memaddrs[0] as usize].0;

	let p2 = stack.pop().unwrap().as_i32();
	let p1 = stack.pop().unwrap().as_i32();

	let p1 = p1 as _;
	let p2 = mem[p2 as usize..].as_mut_ptr() as _;
	let ret = fd_prestat_get(p1, p2);
	stack.push(Val::I32(ret as _));
}

const FD_PRESTAT_GET_DIR_NAME_TYPE: WasiType = (&[ValType::I32, ValType::I32, ValType::I32], &[ValType::I32]);
unsafe fn adapt_fd_prestat_dir_name(s: &mut Store<Interpreter>, inst: &ModuleInst<Interpreter>, stack: &mut Vec<Val>) {
	let mem = &mut s.mems[inst.memaddrs[0] as usize].0;

	let p1 = stack.pop().unwrap().as_i32();
	let p2 = stack.pop().unwrap().as_i32();
	let p3 = stack.pop().unwrap().as_i32();

	let p1 = p1 as _;
	let p2 = mem[p2 as usize..].as_mut_ptr() as _;
	let p3 = p3 as _;
	let ret = fd_prestat_dir_name(p1, p2, p3);
	stack.push(Val::I32(ret as _));
}

const FD_WRITE_TYPE: WasiType = (&[ValType::I32, ValType::I32, ValType::I32, ValType::I32], &[ValType::I32]);
unsafe fn adapt_fd_write(s: &mut Store<Interpreter>, inst: &ModuleInst<Interpreter>, stack: &mut Vec<Val>) {
	let mem = &mut s.mems[inst.memaddrs[0] as usize].0;

	let p1 = stack.pop().unwrap().as_i32();
	let p2 = stack.pop().unwrap().as_i32();
	let p3 = stack.pop().unwrap().as_i32();
	let p4 = stack.pop().unwrap().as_i32();

	let p1 = p1 as _;
	let p2 = mem[p2 as usize..].as_mut_ptr() as _;
	let p3 = p3 as _;
	let p4 = mem[p4 as usize..].as_mut_ptr() as _;
	let ret = fd_write(p1, p2, p3, p4);
	stack.push(Val::I32(ret as _));
}

const PROC_EXIT_TYPE: WasiType = (&[ValType::I32], &[]);
unsafe fn adapt_proc_exit(_s: &mut Store<Interpreter>, _inst: &ModuleInst<Interpreter>, stack: &mut Vec<Val>) {
	let p1 = stack.pop().unwrap().as_i32();

	let p1 = p1 as _;
	proc_exit(p1);
}
