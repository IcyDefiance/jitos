#![no_std]

#[macro_use]
extern crate alloc;
#[macro_use]
extern crate log;

pub mod exec;
pub mod syntax;

mod binary;
mod valid;
