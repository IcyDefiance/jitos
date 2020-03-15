use crate::{
	exec::runtime::{Embedder, ExternVal, Frame, ModuleInst, Store, Val},
	syntax::modules::{ExportDesc, Module},
};
use alloc::{string::ToString, vec::Vec};
use hashbrown::HashMap;

fn alloc_module<E: Embedder>(
	s: &mut Store<E>,
	module: &Module,
	_externs: &[ExternVal<E>],
	vals: Vec<Val>,
) -> ModuleInst<E> {
	let mut moduleinst = ModuleInst::<E> {
		types: module.types().to_vec(),
		funcaddrs: vec![],
		tableaddrs: vec![],
		memaddrs: vec![],
		globaladdrs: vec![],
		exports: HashMap::new(),
	};

	let mut embedder = E::default();

	// TODO: use externs
	moduleinst.funcaddrs = module.funcs.iter().map(|func| embedder.alloc_func(s, func)).collect();
	moduleinst.tableaddrs = module.tables.iter().map(|table| embedder.alloc_table(s, table)).collect();
	moduleinst.memaddrs = module.mems.iter().map(|mem| embedder.alloc_mem(s, mem)).collect();
	moduleinst.globaladdrs =
		module.globals.iter().zip(vals).map(|(global, val)| embedder.alloc_global(s, global, &val)).collect();
	moduleinst.exports = module
		.exports
		.iter()
		.map(|export| {
			(export.name.to_string(), match &export.desc {
				ExportDesc::Func(idx) => ExternVal::Func(moduleinst.funcaddrs[idx.0 as usize].clone()),
				ExportDesc::Table(idx) => ExternVal::Table(moduleinst.tableaddrs[idx.0 as usize].clone()),
				ExportDesc::Mem(idx) => ExternVal::Mem(moduleinst.memaddrs[idx.0 as usize].clone()),
				ExportDesc::Global(idx) => ExternVal::Global(moduleinst.globaladdrs[idx.0 as usize].clone()),
			})
		})
		.collect();

	moduleinst
}

pub fn instantiate<E: Embedder>(
	s: &mut Store<E>,
	module: &mut Module,
	externs: &[ExternVal<E>],
) -> Result<ModuleInst<E>, &'static str> {
	if module.validate().is_err() {
		return Err("invalid module");
	}

	let mut embedder = E::default();

	// TODO: use externs

	let f_im = Frame::new(vec![]);
	embedder.push_frame(f_im);
	let vals: Vec<_> = module.globals.iter().map(|g| embedder.eval(&g.init)).collect();
	embedder.pop_frame();

	let moduleinst = alloc_module(s, module, externs, vals);

	let f_im = Frame::new(vec![]);
	embedder.push_frame(f_im);
	for elem in &module.elem {
		let eoff = embedder.eval(&elem.offset).as_i32();
		let tableidx = &elem.table;
		let tableaddr = &moduleinst.tableaddrs[tableidx.0 as usize];
		for (j, funcidx) in elem.init.iter().enumerate() {
			let funcaddr = moduleinst.funcaddrs[funcidx.0 as usize].clone();
			embedder.set_elem(s, tableaddr.clone(), eoff as usize + j, funcaddr);
		}
	}
	for data in &module.data {
		let doff = embedder.eval(&data.offset).as_i32();
		let memidx = &data.data;
		let memaddr = &moduleinst.memaddrs[memidx.0 as usize];
		embedder.set_data(s, memaddr.clone(), doff, &data.init);
	}
	embedder.pop_frame();

	// TODO: invoke start

	Ok(moduleinst)
}
