use crate::wasm::decoder::{
	instructions::{expr, Expr},
	types::{functype, globaltype, memtype, tabletype, valtype, FuncType, GlobalType, MemType, TableType, ValType},
	values::{name, u32, vec, vec_byte},
};
use alloc::vec::Vec;
use core::{fmt, iter::repeat};
use nom::{
	bytes::streaming::{tag, take},
	combinator::{complete, map, opt, rest},
	error::{make_error, ErrorKind},
	multi::many0,
	number::streaming::le_u8,
	IResult,
};

#[derive(Debug, Default)]
pub struct TypeIdx(u32);
impl TypeIdx {
	pub fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
		write!(f, "{}", self.0)
	}
}
pub fn typeidx(i: &[u8]) -> IResult<&[u8], TypeIdx> {
	map(u32, |idx| TypeIdx(idx))(i)
}

#[derive(Debug, Default)]
pub struct FuncIdx(u32);
impl FuncIdx {
	pub fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
		write!(f, "{}", self.0)
	}
}
pub fn funcidx(i: &[u8]) -> IResult<&[u8], FuncIdx> {
	map(u32, |idx| FuncIdx(idx))(i)
}

#[derive(Debug, Default)]
pub struct TableIdx(u32);
impl TableIdx {
	pub fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
		write!(f, "{}", self.0)
	}
}
fn tableidx(i: &[u8]) -> IResult<&[u8], TableIdx> {
	map(u32, |idx| TableIdx(idx))(i)
}

#[derive(Debug, Default)]
pub struct MemIdx(u32);
impl MemIdx {
	pub fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
		write!(f, "{}", self.0)
	}
}
fn memidx(i: &[u8]) -> IResult<&[u8], MemIdx> {
	map(u32, |idx| MemIdx(idx))(i)
}

#[derive(Debug, Default)]
pub struct GlobalIdx(u32);
impl GlobalIdx {
	pub fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
		write!(f, "{}", self.0)
	}
}
pub fn globalidx(i: &[u8]) -> IResult<&[u8], GlobalIdx> {
	map(u32, |idx| GlobalIdx(idx))(i)
}

#[derive(Debug, Default)]
pub struct LocalIdx(u32);
impl LocalIdx {
	pub fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
		write!(f, "{}", self.0)
	}
}
pub fn localidx(i: &[u8]) -> IResult<&[u8], LocalIdx> {
	map(u32, |idx| LocalIdx(idx))(i)
}

#[derive(Debug, Default)]
pub struct LabelIdx(u32);
impl LabelIdx {
	pub fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
		write!(f, "{}", self.0)
	}
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
		let (_, content) = complete(|i| content(i))(data)?;
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

fn importsec(i: &[u8]) -> IResult<&[u8], ()> {
	let (i, res) = opt(section(2, |i| Ok((i, ()))))(i)?;
	match res {
		Some(_) => unimplemented!(),
		None => Ok((i, ())),
	}
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

#[derive(Debug)]
pub struct Table {
	pub typ: TableType,
}
impl Table {
	fn write_wat(&self, f: &mut dyn fmt::Write, idx: usize) -> fmt::Result {
		write!(f, "(table (;{};) ", idx)?;
		self.typ.write_wat(f)?;
		write!(f, ")")
	}
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

#[derive(Debug)]
pub struct Mem {
	pub typ: MemType,
}
impl Mem {
	fn write_wat(&self, f: &mut dyn fmt::Write, idx: usize) -> fmt::Result {
		write!(f, "(memory (;{};) ", idx)?;
		self.typ.write_wat(f)?;
		write!(f, ")")
	}
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

#[derive(Debug)]
pub struct Global {
	pub typ: GlobalType,
	pub init: Expr,
}
impl Global {
	fn write_wat(&self, f: &mut dyn fmt::Write, idx: usize) -> fmt::Result {
		write!(f, "(global (;{};) (", idx)?;
		self.typ.write_wat(f)?;
		write!(f, " ")?;
		self.init.write_wat(f, 4)?;
		write!(f, "))")
	}
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

#[derive(Debug)]
pub struct Export<'a> {
	pub name: &'a str,
	pub desc: ExportDesc,
}
impl<'a> Export<'a> {
	fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
		write!(f, "(export \"{}\" (", self.name)?;
		self.desc.write_wat(f)?;
		write!(f, "))")
	}
}

fn export(i: &[u8]) -> IResult<&[u8], Export> {
	let (i, name) = name(i)?;
	let (i, desc) = exportdesc(i)?;
	Ok((i, Export { name, desc }))
}

#[derive(Debug)]
pub enum ExportDesc {
	Func(FuncIdx),
	Table(TableIdx),
	Mem(MemIdx),
	Global(GlobalIdx),
}
impl ExportDesc {
	fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
		match self {
			ExportDesc::Func(idx) => {
				write!(f, "func ")?;
				idx.write_wat(f)
			},
			ExportDesc::Table(idx) => {
				write!(f, "table ")?;
				idx.write_wat(f)
			},
			ExportDesc::Mem(idx) => {
				write!(f, "memory ")?;
				idx.write_wat(f)
			},
			ExportDesc::Global(idx) => {
				write!(f, "global ")?;
				idx.write_wat(f)
			},
		}
	}
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

fn startsec(i: &[u8]) -> IResult<&[u8], Start> {
	let (i, start) = opt(section(8, start))(i)?;
	let start = start.unwrap_or(Start::default());
	Ok((i, start))
}

#[derive(Debug, Default)]
pub struct Start {
	pub func: FuncIdx,
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

#[derive(Debug)]
pub struct Elem {
	pub table: TableIdx,
	pub offset: Expr,
	pub inits: Vec<FuncIdx>,
}
impl Elem {
	fn write_wat(&self, f: &mut dyn fmt::Write, idx: usize) -> fmt::Result {
		write!(f, "(elem (;{};) (", idx)?;
		self.offset.write_wat(f, 4)?;
		write!(f, ")")?;
		for init in &self.inits {
			write!(f, " ")?;
			init.write_wat(f)?;
		}
		write!(f, ")")
	}
}

fn elem(i: &[u8]) -> IResult<&[u8], Elem> {
	let (i, table) = tableidx(i)?;
	let (i, offset) = expr(i)?;
	let (i, inits) = vec(funcidx)(i)?;
	Ok((i, Elem { table, offset, inits }))
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
	let (i, datas) = opt(section(10, vec(data)))(i)?;
	let datas = datas.unwrap_or(vec![]);
	Ok((i, datas))
}

#[derive(Debug)]
pub struct Data<'a> {
	data: MemIdx,
	offset: Expr,
	init: &'a [u8],
}

fn data(i: &[u8]) -> IResult<&[u8], Data> {
	let (i, data) = memidx(i)?;
	let (i, offset) = expr(i)?;
	let (i, init) = vec_byte(i)?;
	Ok((i, Data { data, offset, init }))
}

#[derive(Debug)]
pub struct Module<'a> {
	pub customs: Vec<Custom<'a>>,
	pub types: Vec<FuncType>,
	pub funcs: Vec<Func>,
	pub tables: Vec<Table>,
	pub mems: Vec<Mem>,
	pub globals: Vec<Global>,
	pub exports: Vec<Export<'a>>,
	pub start: Start,
	pub elems: Vec<Elem>,
	pub datas: Vec<Data<'a>>,
}
impl<'a> Module<'a> {
	pub fn write_wat(&self, f: &mut dyn fmt::Write) -> fmt::Result {
		if self.customs.len() != 0 {
			panic!("display for Custom not implemented yet");
		}

		write!(f, "(module")?;
		for (idx, typ) in self.types.iter().enumerate() {
			write!(f, "\n  ")?;
			typ.write_wat(f, idx)?;
		}
		for (idx, func) in self.funcs.iter().enumerate() {
			write!(f, "\n  ")?;
			func.write_wat(f, idx)?;
		}
		for (idx, table) in self.tables.iter().enumerate() {
			write!(f, "\n  ")?;
			table.write_wat(f, idx)?;
		}
		for (idx, mem) in self.mems.iter().enumerate() {
			write!(f, "\n  ")?;
			mem.write_wat(f, idx)?;
		}
		for (idx, global) in self.globals.iter().enumerate() {
			write!(f, "\n  ")?;
			global.write_wat(f, idx)?;
		}
		for export in &self.exports {
			write!(f, "\n  ")?;
			export.write_wat(f)?;
		}
		for (idx, elem) in self.elems.iter().enumerate() {
			write!(f, "\n  ")?;
			elem.write_wat(f, idx)?;
		}
		write!(f, ")\n")
	}
}

#[derive(Debug)]
pub struct Func {
	typ: TypeIdx,
	locals: Vec<ValType>,
	body: Expr,
}
impl Func {
	pub fn write_wat(&self, f: &mut dyn fmt::Write, idx: usize) -> fmt::Result {
		write!(f, "(func (;{};) (type ", idx)?;
		self.typ.write_wat(f)?;
		write!(f, ")")?;
		if self.locals.len() > 0 {
			write!(f, "\n    (local")?;
			for local in &self.locals {
				write!(f, " ")?;
				local.write_wat(f)?;
			}
			write!(f, ")\n    ")?;
		}
		self.body.write_wat(f, 4)?;
		write!(f, ")")?;
		Ok(())
	}
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
	let (i, _) = importsec(i)?; // ignoring for now, because I don't have a test case
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
	let (i, elems) = elemsec(i)?;
	let (i, _) = append_customs(i)?;
	let (i, codes) = codesec(i)?;
	let (i, _) = append_customs(i)?;
	let (i, datas) = datasec(i)?;
	let (i, _) = append_customs(i)?;

	let funcs = funcs.into_iter().zip(codes).map(|(typ, (locals, body))| Func { typ, locals, body }).collect();

	Ok((i, Module { customs, types, funcs, tables, mems, globals, exports, start, elems, datas }))
}

fn magic(i: &[u8]) -> IResult<&[u8], &[u8]> {
	tag(b"\0asm")(i)
}

fn version(i: &[u8]) -> IResult<&[u8], &[u8]> {
	tag(&[0x01, 0x00, 0x00, 0x00])(i)
}
