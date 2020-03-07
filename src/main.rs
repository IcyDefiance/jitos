#![no_std]
#![no_main]
#![feature(custom_test_frameworks)]
#![test_runner(jitos::test_runner)]
#![reexport_test_harness_main = "test_main"]

extern crate alloc;
extern crate alloc as std;

use bootloader::{entry_point, BootInfo};
use core::panic::PanicInfo;
use jitos::{println, wasm::test_wasm};

entry_point!(kernel_main);

fn kernel_main(_boot_info: &'static BootInfo) -> ! {
	jitos::init();

	// let phys_mem_offset = VirtAddr::new(boot_info.physical_memory_offset);
	// let mut mapper = unsafe { memory::init(phys_mem_offset) };
	// let mut frame_allocator = unsafe { BootInfoFrameAllocator::init(&boot_info.memory_map) };

	test_wasm();

	#[cfg(test)]
	test_main();

	// println!("It did not crash!");
	jitos::hlt_loop();
}

/// This function is called on panic.
#[cfg(not(test))]
#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
	println!("{}", info);
	jitos::hlt_loop();
}

#[cfg(test)]
#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
	jitos::test_panic_handler(info)
}
