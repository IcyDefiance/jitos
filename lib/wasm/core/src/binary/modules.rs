use crate::{
	binary::{
		instructions::expr,
		types::{functype, globaltype, memtype, tabletype, valtype},
		values::{name, u32, vec, vec_byte},
	},
	syntax::{
		instructions::Expr,
		modules::{
			Data, Elem, Export, ExportDesc, Func, FuncIdx, Global, GlobalIdx, Import, ImportDesc, LabelIdx, LocalIdx,
			Mem, MemIdx, Module, Start, Table, TableIdx, TypeIdx,
		},
		types::{FuncType, ValType},
	},
};
use alloc::vec::Vec;
use core::iter::repeat;
use nom::{
	bytes::complete::{tag, take},
	combinator::{all_consuming, complete, map, opt, rest},
	error::{make_error, ErrorKind},
	multi::many0,
	number::complete::le_u8,
	IResult,
};

pub fn typeidx(i: &[u8]) -> IResult<&[u8], TypeIdx> {
	map(u32, |idx| TypeIdx(idx))(i)
}

pub fn funcidx(i: &[u8]) -> IResult<&[u8], FuncIdx> {
	map(u32, |idx| FuncIdx(idx))(i)
}

fn tableidx(i: &[u8]) -> IResult<&[u8], TableIdx> {
	map(u32, |idx| TableIdx(idx))(i)
}

fn memidx(i: &[u8]) -> IResult<&[u8], MemIdx> {
	map(u32, |idx| MemIdx(idx))(i)
}

pub fn globalidx(i: &[u8]) -> IResult<&[u8], GlobalIdx> {
	map(u32, |idx| GlobalIdx(idx))(i)
}

pub fn localidx(i: &[u8]) -> IResult<&[u8], LocalIdx> {
	map(u32, |idx| LocalIdx(idx))(i)
}

pub fn labelidx(i: &[u8]) -> IResult<&[u8], LabelIdx> {
	map(u32, |idx| LabelIdx(idx))(i)
}

fn section<'a, B>(
	id: u8,
	content: impl Fn(&'a [u8]) -> IResult<&'a [u8], B>,
) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], B> {
	move |i| {
		let (i, _) = tag(&[id])(i)?;
		let (i, size) = u32(i)?;
		let (i, data) = take(size)(i)?;
		let (_, content) = all_consuming(&content)(data)?;
		Ok((i, content))
	}
}

fn customsec(i: &[u8]) -> IResult<&[u8], Custom> {
	section(0, custom)(i)
}

#[derive(Debug)]
pub struct Custom<'a> {
	pub name: &'a str,
	pub data: &'a [u8],
}

fn custom(i: &[u8]) -> IResult<&[u8], Custom> {
	let (i, name) = name(i)?;
	let (i, data) = rest(i)?;
	Ok((i, Custom { name, data }))
}

fn typesec(i: &[u8]) -> IResult<&[u8], Vec<FuncType>> {
	let (i, functypes) = opt(section(1, vec(functype)))(i)?;
	let functypes = functypes.unwrap_or(vec![]);
	Ok((i, functypes))
}

fn importsec(i: &[u8]) -> IResult<&[u8], Vec<Import>> {
	let (i, imports) = opt(section(2, vec(import)))(i)?;
	let imports = imports.unwrap_or(vec![]);
	Ok((i, imports))
}

fn import(i: &[u8]) -> IResult<&[u8], Import> {
	let (i, module) = name(i)?;
	let (i, name) = name(i)?;
	let (i, desc) = importdesc(i)?;
	Ok((i, Import { module, name, desc }))
}

fn importdesc(i: &[u8]) -> IResult<&[u8], ImportDesc> {
	let (i, flag) = le_u8(i)?;
	let (i, desc) = match flag {
		0x00 => {
			let (i, idx) = typeidx(i)?;
			(i, ImportDesc::Func(idx))
		},
		0x01 => {
			let (i, idx) = tabletype(i)?;
			(i, ImportDesc::Table(idx))
		},
		0x02 => {
			let (i, idx) = memtype(i)?;
			(i, ImportDesc::Mem(idx))
		},
		0x03 => {
			let (i, idx) = globaltype(i)?;
			(i, ImportDesc::Global(idx))
		},
		_ => return Err(nom::Err::Error(make_error(i, ErrorKind::Switch))),
	};
	Ok((i, desc))
}

fn funcsec(i: &[u8]) -> IResult<&[u8], Vec<TypeIdx>> {
	let (i, typeidxs) = opt(section(3, vec(typeidx)))(i)?;
	let typeidxs = typeidxs.unwrap_or(vec![]);
	Ok((i, typeidxs))
}

fn tablesec(i: &[u8]) -> IResult<&[u8], Vec<Table>> {
	let (i, tables) = opt(section(4, vec(table)))(i)?;
	let tables = tables.unwrap_or(vec![]);
	Ok((i, tables))
}

fn table(i: &[u8]) -> IResult<&[u8], Table> {
	let (i, typ) = tabletype(i)?;
	Ok((i, Table { typ }))
}

fn memsec(i: &[u8]) -> IResult<&[u8], Vec<Mem>> {
	let (i, mems) = opt(section(5, vec(mem)))(i)?;
	let mems = mems.unwrap_or(vec![]);
	Ok((i, mems))
}

fn mem(i: &[u8]) -> IResult<&[u8], Mem> {
	let (i, typ) = memtype(i)?;
	Ok((i, Mem { typ }))
}

fn globalsec(i: &[u8]) -> IResult<&[u8], Vec<Global>> {
	let (i, globals) = opt(section(6, vec(global)))(i)?;
	let globals = globals.unwrap_or(vec![]);
	Ok((i, globals))
}

fn global(i: &[u8]) -> IResult<&[u8], Global> {
	let (i, typ) = globaltype(i)?;
	let (i, init) = expr(i)?;
	Ok((i, Global { typ, init }))
}

fn exportsec(i: &[u8]) -> IResult<&[u8], Vec<Export>> {
	let (i, exports) = opt(section(7, vec(export)))(i)?;
	let exports = exports.unwrap_or(vec![]);
	Ok((i, exports))
}

fn export(i: &[u8]) -> IResult<&[u8], Export> {
	let (i, name) = name(i)?;
	let (i, desc) = exportdesc(i)?;
	Ok((i, Export { name, desc }))
}

fn exportdesc(i: &[u8]) -> IResult<&[u8], ExportDesc> {
	let (i, flag) = le_u8(i)?;
	let (i, desc) = match flag {
		0x00 => {
			let (i, idx) = funcidx(i)?;
			(i, ExportDesc::Func(idx))
		},
		0x01 => {
			let (i, idx) = tableidx(i)?;
			(i, ExportDesc::Table(idx))
		},
		0x02 => {
			let (i, idx) = memidx(i)?;
			(i, ExportDesc::Mem(idx))
		},
		0x03 => {
			let (i, idx) = globalidx(i)?;
			(i, ExportDesc::Global(idx))
		},
		_ => return Err(nom::Err::Error(make_error(i, ErrorKind::Switch))),
	};
	Ok((i, desc))
}

fn startsec(i: &[u8]) -> IResult<&[u8], Option<Start>> {
	let (i, start) = opt(section(8, start))(i)?;
	Ok((i, start))
}

fn start(i: &[u8]) -> IResult<&[u8], Start> {
	let (i, func) = funcidx(i)?;
	Ok((i, Start { func }))
}

fn elemsec(i: &[u8]) -> IResult<&[u8], Vec<Elem>> {
	let (i, elems) = opt(section(9, vec(elem)))(i)?;
	let elems = elems.unwrap_or(vec![]);
	Ok((i, elems))
}

fn elem(i: &[u8]) -> IResult<&[u8], Elem> {
	let (i, table) = tableidx(i)?;
	let (i, offset) = expr(i)?;
	let (i, init) = vec(funcidx)(i)?;
	Ok((i, Elem { table, offset, init }))
}

fn codesec(i: &[u8]) -> IResult<&[u8], Vec<(Vec<ValType>, Expr)>> {
	let (i, codes) = opt(section(10, vec(code)))(i)?;
	let codes = codes.unwrap_or(vec![]);
	Ok((i, codes))
}

fn code(i: &[u8]) -> IResult<&[u8], (Vec<ValType>, Expr)> {
	let (i, size) = u32(i)?;
	let (i, data) = take(size)(i)?;
	let (_, code) = complete(func)(data)?;
	Ok((i, code))
}

fn func(i: &[u8]) -> IResult<&[u8], (Vec<ValType>, Expr)> {
	let (i, locals) = vec(locals)(i)?;
	let locals = locals.into_iter().flatten().collect();
	let (i, body) = expr(i)?;
	Ok((i, (locals, body)))
}

fn locals(i: &[u8]) -> IResult<&[u8], impl Iterator<Item = ValType>> {
	let (i, len) = u32(i)?;
	let (i, typ) = valtype(i)?;
	Ok((i, repeat(typ).take(len as _)))
}

fn datasec(i: &[u8]) -> IResult<&[u8], Vec<Data>> {
	let (i, datas) = opt(section(11, vec(data)))(i)?;
	let datas = datas.unwrap_or(vec![]);
	Ok((i, datas))
}

fn data(i: &[u8]) -> IResult<&[u8], Data> {
	let (i, data) = memidx(i)?;
	let (i, offset) = expr(i)?;
	let (i, init) = vec_byte(i)?;
	Ok((i, Data { data, offset, init }))
}

pub fn module(i: &[u8]) -> IResult<&[u8], Module> {
	let mut customs = vec![];
	let mut append_customs = |i| -> IResult<&[u8], ()> {
		let (i, customs2) = many0(customsec)(i as &[u8])?;
		customs.extend(customs2);
		Ok((i, ()))
	};

	let (i, _) = magic(i)?;
	let (i, _) = version(i)?;
	let (i, _) = append_customs(i)?;
	let (i, types) = typesec(i)?;
	let (i, _) = append_customs(i)?;
	let (i, imports) = importsec(i)?;
	let (i, _) = append_customs(i)?;
	let (i, funcs) = funcsec(i)?;
	let (i, _) = append_customs(i)?;
	let (i, tables) = tablesec(i)?;
	let (i, _) = append_customs(i)?;
	let (i, mems) = memsec(i)?;
	let (i, _) = append_customs(i)?;
	let (i, globals) = globalsec(i)?;
	let (i, _) = append_customs(i)?;
	let (i, exports) = exportsec(i)?;
	let (i, _) = append_customs(i)?;
	let (i, start) = startsec(i)?;
	let (i, _) = append_customs(i)?;
	let (i, elem) = elemsec(i)?;
	let (i, _) = append_customs(i)?;
	let (i, codes) = codesec(i)?;
	let (i, _) = append_customs(i)?;
	let (i, data) = datasec(i)?;
	let (i, _) = append_customs(i)?;

	let funcs = funcs.into_iter().zip(codes).map(|(typ, (locals, body))| Func { typ, locals, body }).collect();

	Ok((i, Module::new(types, funcs, tables, mems, globals, elem, data, start, imports, exports, customs)))
}

fn magic(i: &[u8]) -> IResult<&[u8], &[u8]> {
	tag(b"\0asm")(i)
}

fn version(i: &[u8]) -> IResult<&[u8], &[u8]> {
	tag(&[0x01, 0x00, 0x00, 0x00])(i)
}
