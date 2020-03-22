use crate::{
	syntax::{
		modules::{Data, Elem, Export, ExportDesc, Func, Global, Import, ImportDesc, Mem, Module, Start, Table},
		types::{ElemType, ResultType, ValType},
	},
	valid::{
		instructions::validate_expr,
		types::{validate_functype, validate_globaltype, validate_memtype, validate_tabletype},
		Context,
	},
};
use hashbrown::HashSet;

fn validate_func(c: &Context, func: &Func) -> Result<(), &'static str> {
	let typ = func.typ.0 as _;
	if c.types.len() <= typ {
		return Err("func: The type must be defined in the context.");
	}

	let functype = &c.types[typ];
	let cp = Context {
		locals: [&functype.params[..], &func.locals[..]].concat(),
		labels: vec![ResultType(functype.results.get(0).cloned())],
		retur: Some(ResultType(functype.results.get(0).cloned())),
		..c.clone()
	};

	validate_expr(&cp, &ResultType(functype.results.get(0).cloned()), &func.body, false)?;

	Ok(())
}

fn validate_table(table: &Table) -> Result<(), &'static str> {
	validate_tabletype(&table.typ)?;
	Ok(())
}

fn validate_mem(mem: &Mem) -> Result<(), &'static str> {
	validate_memtype(&mem.typ)?;
	Ok(())
}

fn validate_global(c: &Context, global: &Global) -> Result<(), &'static str> {
	validate_globaltype(&global.typ)?;
	validate_expr(&c, &ResultType(Some(global.typ.valtype)), &global.init, true)?;
	Ok(())
}

fn validate_elem(c: &Context, elem: &Elem) -> Result<(), &'static str> {
	let tablei = elem.table.0 as usize;
	if tablei as usize >= c.tables.len() {
		return Err("undefined table");
	}
	let table = &c.tables[tablei as usize];
	if table.et != ElemType::FuncRef {
		return Err("invalid table");
	}
	validate_expr(&c, &ResultType(Some(ValType::I32)), &elem.offset, true)?;
	for func in &elem.init {
		if func.0 as usize >= c.funcs.len() {
			return Err("undefined func");
		}
	}
	Ok(())
}

fn validate_data(c: &Context, data: &Data) -> Result<(), &'static str> {
	let memi = data.data.0 as usize;
	if memi as usize >= c.mems.len() {
		return Err("undefined mem");
	}
	validate_expr(&c, &ResultType(Some(ValType::I32)), &data.offset, true)?;
	Ok(())
}

fn validate_start(c: &Context, start: &Start) -> Result<(), &'static str> {
	let funci = start.func.0 as usize;
	if funci as usize >= c.funcs.len() {
		return Err("undefined func");
	}
	let func = &c.funcs[funci];
	if func.params.len() > 0 || func.results.len() > 0 {
		return Err("invalid start func");
	}
	Ok(())
}

fn validate_import(c: &Context, import: &Import) -> Result<(), &'static str> {
	validate_importdesc(c, &import.desc)?;
	Ok(())
}

fn validate_importdesc(c: &Context, importdesc: &ImportDesc) -> Result<(), &'static str> {
	match importdesc {
		ImportDesc::Func(typeidx) => {
			if typeidx.0 as usize >= c.types.len() {
				return Err("undefined type");
			}
		},
		ImportDesc::Table(tabletype) => validate_tabletype(tabletype)?,
		ImportDesc::Mem(memtype) => validate_memtype(memtype)?,
		ImportDesc::Global(globaltype) => validate_globaltype(globaltype)?,
	}
	Ok(())
}

fn validate_export(c: &Context, export: &Export) -> Result<(), &'static str> {
	validate_exportdesc(c, &export.desc)?;
	Ok(())
}

fn validate_exportdesc(c: &Context, exportdesc: &ExportDesc) -> Result<(), &'static str> {
	match exportdesc {
		ExportDesc::Func(funcidx) => {
			if funcidx.0 as usize >= c.funcs.len() {
				return Err("undefined func");
			}
		},
		ExportDesc::Table(tableidx) => {
			if tableidx.0 as usize >= c.tables.len() {
				return Err("undefined table");
			}
		},
		ExportDesc::Mem(memidx) => {
			if memidx.0 as usize >= c.mems.len() {
				return Err("undefined mem");
			}
		},
		ExportDesc::Global(globalidx) => {
			if globalidx.0 as usize >= c.globals.len() {
				return Err("undefined global");
			}
		},
	}
	Ok(())
}

pub fn validate_module(module: &Module) -> Result<(), &'static str> {
	// TODO: concat imports in funcs, tables, mems, and globals
	let c = Context {
		types: module.types().iter().collect(),
		funcs: module
			.imports()
			.iter()
			.filter_map(|i| i.desc.as_func())
			.chain(module.funcs().iter().map(|f| f.typ))
			.map(|typ| &module.types()[typ.0 as usize])
			.collect(),
		tables: module
			.imports()
			.iter()
			.filter_map(|i| i.desc.as_table())
			.chain(module.tables().iter().map(|t| &t.typ))
			.collect(),
		mems: module
			.imports()
			.iter()
			.filter_map(|i| i.desc.as_mem())
			.chain(module.mems().iter().map(|t| &t.typ))
			.collect(),
		globals: module
			.imports()
			.iter()
			.filter_map(|i| i.desc.as_global())
			.chain(module.globals().iter().map(|t| &t.typ))
			.collect(),
		locals: vec![],
		labels: vec![],
		retur: None,
	};

	let cp =
		Context { globals: module.imports().iter().filter_map(|i| i.desc.as_global()).collect(), ..Context::default() };

	for functype in module.types() {
		validate_functype(functype)?;
	}
	for func in module.funcs() {
		validate_func(&c, func)?;
	}
	for table in module.tables() {
		validate_table(table)?;
	}
	for mem in module.mems() {
		validate_mem(mem)?;
	}
	for global in module.globals() {
		validate_global(&cp, global)?;
	}
	for elem in module.elem() {
		validate_elem(&c, elem)?;
	}
	for data in module.data() {
		validate_data(&c, data)?;
	}
	if let Some(start) = module.start() {
		validate_start(&c, start)?;
	}
	for import in module.imports() {
		validate_import(&c, import)?;
	}
	{
		let mut names = HashSet::new();
		for export in module.exports() {
			validate_export(&c, export)?;
			if !names.insert(export.name) {
				return Err("module: All export names must be different.");
			}
		}
	}

	if c.tables.len() > 1 {
		return Err("module: The length of tables must not be larger than 1.");
	}
	if c.mems.len() > 1 {
		return Err("module: The length of mems must not be larger than 1.");
	}

	Ok(())
}
