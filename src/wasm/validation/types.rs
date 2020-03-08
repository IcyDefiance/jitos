use crate::wasm::structure::types::{FuncType, GlobalType, Limits, MemType, TableType};
use core::{u16, u32};

pub fn validate_limits(limits: &Limits, range: u32) -> Result<(), &'static str> {
	if limits.min > range {
		return Err("limits: min > range");
	}
	if let Some(max) = limits.max {
		if max > range {
			return Err("limits: max > range");
		}
		if max < limits.min {
			return Err("limits: max < min");
		}
	}
	Ok(())
}

pub fn validate_functype(functype: &FuncType) -> Result<(), &'static str> {
	if functype.results.len() > 1 {
		return Err("Function types may not specify more than one result.");
	}
	Ok(())
}

pub fn validate_tabletype(tabletype: &TableType) -> Result<(), &'static str> {
	validate_limits(&tabletype.lim, u32::MAX)?;
	Ok(())
}

pub fn validate_memtype(memtype: &MemType) -> Result<(), &'static str> {
	validate_limits(&memtype.lim, u16::MAX as _)?;
	Ok(())
}

pub fn validate_globaltype(_globaltype: &GlobalType) -> Result<(), &'static str> {
	Ok(())
}
