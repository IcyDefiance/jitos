use core::{fmt, fmt::Write};
use lazy_static::lazy_static;
use spin::Mutex;
use volatile::Volatile;
use x86_64::instructions::interrupts;

#[cfg(test)]
use crate::{serial_print, serial_println};

lazy_static! {
	/// A global `Writer` instance that can be used for printing to the VGA text buffer.
	///
	/// Used by the `print!` and `println!` macros.
	pub static ref WRITER: Mutex<Writer> = Mutex::new(Writer {
		colpos: 0,
		color: VgaColor::new(Color::LightGray, Color::Black),
		buffer: unsafe { &mut *(0xb8000 as *mut Buffer) },
	});
}

/// The standard color palette in VGA text mode.
#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Color {
	Black = 0,
	Blue = 1,
	Green = 2,
	Cyan = 3,
	Red = 4,
	Magenta = 5,
	Brown = 6,
	LightGray = 7,
	DarkGray = 8,
	LightBlue = 9,
	LightGreen = 10,
	LightCyan = 11,
	LightRed = 12,
	Pink = 13,
	Yellow = 14,
	White = 15,
}

/// A combination of a foreground and a background color.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
struct VgaColor(u8);
impl VgaColor {
	/// Create a new `VgaColor` with the given foreground and background colors.
	fn new(foreground: Color, background: Color) -> Self {
		Self((background as u8) << 4 | (foreground as u8))
	}
}

/// A screen character in the VGA text buffer, consisting of an ASCII character and a `VgaColor`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
struct ScreenChar {
	ch: u8,
	color: VgaColor,
}

/// The height of the text buffer (normally 25 lines).
const BUFFER_HEIGHT: usize = 25;
/// The width of the text buffer (normally 80 columns).
const BUFFER_WIDTH: usize = 80;

/// A structure representing the VGA text buffer.
#[repr(transparent)]
struct Buffer {
	chars: [[Volatile<ScreenChar>; BUFFER_WIDTH]; BUFFER_HEIGHT],
}

/// A writer type that allows writing ASCII bytes and strings to an underlying `Buffer`.
///
/// Wraps lines at `BUFFER_WIDTH`. Supports newline characters and implements the
/// `core::fmt::Write` trait.
pub struct Writer {
	colpos: usize,
	color: VgaColor,
	buffer: &'static mut Buffer,
}
impl Writer {
	/// Writes an ASCII byte to the buffer.
	///
	/// Wraps lines at `BUFFER_WIDTH`. Supports the `\n` newline character.
	pub fn write_byte(&mut self, byte: u8) {
		match byte {
			b'\n' => self.new_line(),
			byte => {
				if self.colpos >= BUFFER_WIDTH {
					self.new_line();
				}

				let row = BUFFER_HEIGHT - 1;
				let col = self.colpos;

				let color = self.color;
				self.buffer.chars[row][col].write(ScreenChar { ch: byte, color });
				self.colpos += 1;
			},
		}
	}

	/// Writes the given ASCII string to the buffer.
	///
	/// Wraps lines at `BUFFER_WIDTH`. Supports the `\n` newline character. Does **not**
	/// support strings with non-ASCII characters, since they can't be printed in the VGA text
	/// mode.
	fn write_string(&mut self, s: &str) {
		for byte in s.bytes() {
			match byte {
				// printable ASCII byte or newline
				0x20..=0x7e | b'\n' => self.write_byte(byte),
				// not part of printable ASCII range
				_ => self.write_byte(0xfe),
			}
		}
	}

	/// Shifts all lines one line up and clears the last row.
	fn new_line(&mut self) {
		for row in 1..BUFFER_HEIGHT {
			for col in 0..BUFFER_WIDTH {
				let character = self.buffer.chars[row][col].read();
				self.buffer.chars[row - 1][col].write(character);
			}
		}
		self.clear_row(BUFFER_HEIGHT - 1);
		self.colpos = 0;
	}

	/// Clears a row by overwriting it with blank characters.
	fn clear_row(&mut self, row: usize) {
		let blank = ScreenChar { ch: b' ', color: self.color };
		for col in 0..BUFFER_WIDTH {
			self.buffer.chars[row][col].write(blank);
		}
	}
}
impl fmt::Write for Writer {
	fn write_str(&mut self, s: &str) -> fmt::Result {
		self.write_string(s);
		Ok(())
	}
}

/// Like the `print!` macro in the standard library, but prints to the VGA text buffer.
#[macro_export]
macro_rules! print {
    ($($arg:tt)*) => ($crate::vga_buffer::_print(format_args!($($arg)*)));
}

/// Like the `println!` macro in the standard library, but prints to the VGA text buffer.
#[macro_export]
macro_rules! println {
    () => ($crate::print!("\n"));
    ($($arg:tt)*) => ($crate::print!("{}\n", format_args!($($arg)*)));
}

/// Prints the given formatted string to the VGA text buffer
/// through the global `WRITER` instance.
#[doc(hidden)]
pub fn _print(args: fmt::Arguments) {
	interrupts::without_interrupts(|| WRITER.lock().write_fmt(args).unwrap());
}

#[test_case]
fn test_println_simple() {
	serial_print!("test_println... ");
	println!("test_println_simple output");
	serial_println!("[ok]");
}

#[test_case]
fn test_println_many() {
	serial_print!("test_println_many... ");
	for _ in 0..200 {
		println!("test_println_many output");
	}
	serial_println!("[ok]");
}

#[test_case]
fn test_println_output() {
	serial_print!("test_println_output... ");

	let s = "Some test string that fits on a single line";
	interrupts::without_interrupts(|| {
		let mut writer = WRITER.lock();
		writeln!(writer, "\n{}", s).expect("writeln failed");
		for (i, c) in s.chars().enumerate() {
			let screen_char = writer.buffer.chars[BUFFER_HEIGHT - 2][i].read();
			assert_eq!(char::from(screen_char.ch), c);
		}
	});

	serial_println!("[ok]");
}
