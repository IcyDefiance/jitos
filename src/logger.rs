//! A logger that prints all messages with a readable output format.

extern crate log;

use crate::println;
use alloc::string::ToString;
use lazy_static::lazy_static;
use log::{Level, Log, Metadata, Record, SetLoggerError};

lazy_static! {
	static ref LOGGER: Logger = Logger { level: Level::max() };
}

struct Logger {
	level: Level,
}
impl Log for Logger {
	fn enabled(&self, metadata: &Metadata) -> bool {
		metadata.level() <= self.level
	}

	fn log(&self, record: &Record) {
		if self.enabled(record.metadata()) {
			let color = match record.level() {
				Level::Error => "\x1b[0;31m",
				Level::Info => "\x1b[0;32m",
				Level::Warn => "\x1b[0;33m",
				Level::Debug => "\x1b[0;36m",
				_ => "",
			};
			let nc = "\x1b[0m";
			let level_string = record.level().to_string();
			let target =
				if record.target().len() > 0 { record.target() } else { record.module_path().unwrap_or_default() };
			println!("{}{:<5}{} [{}] {}", color, level_string, nc, target, record.args());
		}
	}

	fn flush(&self) {}
}

pub fn init() -> Result<(), SetLoggerError> {
	log::set_logger(&*LOGGER)?;
	log::set_max_level(LOGGER.level.to_level_filter());
	Ok(())
}
