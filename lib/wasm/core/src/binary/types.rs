use crate::{
	binary::values::{u32, vec},
	syntax::types::{ElemType, FuncType, GlobalType, Limits, MemType, Mut, ResultType, TableType, ValType},
};
use core::convert::TryFrom;
use nom::{
	branch::alt,
	bytes::streaming::tag,
	combinator::{map, map_res, value},
	error::{make_error, ErrorKind},
	number::streaming::le_u8,
	IResult,
};

pub fn valtype(i: &[u8]) -> IResult<&[u8], ValType> {
	map_res(le_u8, |x| ValType::try_from(x))(i)
}

pub fn blocktype(i: &[u8]) -> IResult<&[u8], ResultType> {
	let empty = value(ResultType(None), tag(&[0x40]));
	let typ = map(map_res(le_u8, |x| ValType::try_from(x)), |x| ResultType(Some(x)));
	alt((empty, typ))(i)
}

pub fn functype(i: &[u8]) -> IResult<&[u8], FuncType> {
	let (i, _) = tag(&[0x60])(i)?;
	let (i, params) = vec(valtype)(i)?;
	let (i, results) = vec(valtype)(i)?;
	Ok((i, FuncType { params, results }))
}

fn limits(i: &[u8]) -> IResult<&[u8], Limits> {
	let (i, flag) = le_u8(i)?;
	let (i, min) = u32(i)?;
	let ret = match flag {
		0x00 => (i, Limits { min, max: None }),
		0x01 => {
			let (i, max) = u32(i)?;
			(i, Limits { min, max: Some(max) })
		},
		_ => return Err(nom::Err::Error(make_error(i, ErrorKind::Switch))),
	};
	Ok(ret)
}

pub fn memtype(i: &[u8]) -> IResult<&[u8], MemType> {
	let (i, lim) = limits(i)?;
	Ok((i, MemType { lim }))
}

pub fn tabletype(i: &[u8]) -> IResult<&[u8], TableType> {
	let (i, et) = elemtype(i)?;
	let (i, lim) = limits(i)?;
	Ok((i, TableType { lim, et }))
}

fn elemtype(i: &[u8]) -> IResult<&[u8], ElemType> {
	map_res(le_u8, |x| ElemType::try_from(x))(i)
}

pub fn globaltype(i: &[u8]) -> IResult<&[u8], GlobalType> {
	let (i, valtype) = valtype(i)?;
	let (i, muta) = muta(i)?;
	Ok((i, GlobalType { valtype, muta }))
}

fn muta(i: &[u8]) -> IResult<&[u8], Mut> {
	map_res(le_u8, |x| Mut::try_from(x))(i)
}
