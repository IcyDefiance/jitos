use alloc::vec::Vec;
use core::str;
use nom::{
	bytes::complete::take,
	combinator::{map_res, verify},
	multi::count,
	number::complete::le_u8,
	IResult,
};

pub fn u32(mut i: &[u8]) -> IResult<&[u8], u32> {
	let mut result = 0;
	let mut shift = 0;
	for idx in 0.. {
		let (rem, byte) = verify(le_u8, |b| idx < 4 || *b <= 0xF)(i)?;
		i = rem;
		let byte = byte as u32;
		result |= (byte & 0x7F) << shift;
		if byte & 0x80 == 0 {
			break;
		}
		shift += 7;
	}
	Ok((i, result))
}

pub fn i32(mut i: &[u8]) -> IResult<&[u8], i32> {
	let mut result = 0;
	let mut shift = 0;
	for idx in 0.. {
		let (rem, byte) = verify(le_u8, |b| idx < 4 || (idx == 4 && valid_signed_end(*b, 4)))(i)?;
		i = rem;
		let byte = byte as i32;

		result |= (byte & 0x7F) << shift;
		shift += 7;
		if byte & 0x80 == 0 {
			if shift < 32 && byte & 0x40 > 0 {
				result |= !0 << shift;
			}
			break;
		}
	}
	Ok((i, result))
}

pub fn i64(mut i: &[u8]) -> IResult<&[u8], i64> {
	let mut result = 0;
	let mut shift = 0;
	for idx in 0.. {
		let (rem, byte) = verify(le_u8, |b| idx < 9 || (idx == 9 && valid_signed_end(*b, 1)))(i)?;
		i = rem;
		let byte = byte as i64;

		result |= (byte & 0x7F) << shift;
		shift += 7;
		if byte & 0x80 == 0 {
			if shift < 64 && byte & 0x40 > 0 {
				result |= !0 << shift;
			}
			break;
		}
	}
	Ok((i, result))
}

fn valid_signed_end(b: u8, used_bits: i8) -> bool {
	let sign_and_unused = (b << 1) as i8 >> used_bits;
	sign_and_unused == 0 || sign_and_unused == -1
}

pub fn name(i: &[u8]) -> IResult<&[u8], &str> {
	map_res(vec_byte, |bytes| str::from_utf8(&bytes))(i)
}

pub fn vec<'a, B>(parser: impl Fn(&'a [u8]) -> IResult<&'a [u8], B>) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Vec<B>> {
	move |i| {
		let (i, size) = u32(i)?;
		count(|i| parser(i), size as _)(i)
	}
}

pub fn vec_byte(i: &[u8]) -> IResult<&[u8], &[u8]> {
	let (i, size) = u32(i)?;
	take(size)(i)
}
