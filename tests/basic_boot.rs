#![no_std]
#![no_main]
#![feature(custom_test_frameworks)]
#![test_runner(jitos::test_runner)]
#![reexport_test_harness_main = "test_main"]

use core::panic::PanicInfo;
use jitos::{println, serial_print, serial_println};

#[no_mangle] // don't mangle the name of this function
pub extern fn _start() -> ! {
	test_main();

	loop {}
}

#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
	jitos::test_panic_handler(info)
}

#[test_case]
fn test_println() {
	serial_print!("test_println... ");
	println!("test_println output");
	serial_println!("[ok]");
}