mod instructions;
mod modules;
mod types;
mod values;

use crate::{println, vga_buffer::WRITER};
use modules::module;

pub fn decode(bytes: &[u8]) {
	let module = module(bytes).unwrap().1;
	module.write_wat(&mut *WRITER.lock()).unwrap();
	println!("{:?}", module.start);
}
