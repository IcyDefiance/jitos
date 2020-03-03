#![no_std]
#![no_main]
#![feature(custom_test_frameworks)]
#![test_runner(jitos::test_runner)]
#![reexport_test_harness_main = "test_main"]

extern crate alloc;
extern crate alloc as std;

use alloc::{boxed::Box, rc::Rc, vec, vec::Vec};
use bootloader::{entry_point, BootInfo};
use core::panic::PanicInfo;
use jitos::{
	elf::test_elf,
	memory::{self, BootInfoFrameAllocator},
	println,
};
use x86_64::VirtAddr;

entry_point!(kernel_main);

fn kernel_main(boot_info: &'static BootInfo) -> ! {
	println!("Hello World{}", "!");
	jitos::init();

	let phys_mem_offset = VirtAddr::new(boot_info.physical_memory_offset);
	let mut mapper = unsafe { memory::init(phys_mem_offset) };
	let mut frame_allocator = unsafe { BootInfoFrameAllocator::init(&boot_info.memory_map) };

	// allocate a number on the heap
	let heap_value = Box::new(41);
	println!("heap_value at {:p}", heap_value);

	// create a dynamically sized vector
	let mut vec = Vec::new();
	for i in 0..500 {
		vec.push(i);
	}
	println!("vec at {:p}", vec.as_slice());

	// create a reference counted vector -> will be freed when count reaches 0
	let reference_counted = Rc::new(vec![1, 2, 3]);
	let cloned_reference = reference_counted.clone();
	println!("current reference count is {}", Rc::strong_count(&cloned_reference));
	core::mem::drop(reference_counted);
	println!("reference count is {} now", Rc::strong_count(&cloned_reference));

	test_elf(&mut mapper, &mut frame_allocator);

	#[cfg(test)]
	test_main();

	println!("It did not crash!");
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
