use crate::{print, println};
use alloc::vec::Vec;
use core::{
	mem::{size_of, transmute},
	slice, str,
};
use goblin::{
	elf::note::*,
	elf64::{dynamic::*, header::*, program_header::*, section_header::*},
};
use x86_64::{
	structures::paging::{mapper::MapToError, FrameAllocator, Mapper, Page, PageTableFlags, Size4KiB},
	VirtAddr,
};

pub const ELF_START: usize = 0x_4444_4444_0000;
pub const ELF_SIZE: usize = 1 * 1024 * 1024; // 1 MiB

pub fn test_elf(mapper: &mut impl Mapper<Size4KiB>, frame_allocator: &mut impl FrameAllocator<Size4KiB>) {
	let binary = include_bytes!("../target/debug/layer-std");

	let header = unsafe { &*(binary.as_ptr() as *const Header) };

	// 0x7f, "ELF", ELFCLASS64, ELFDATA2LSB, EV_CURRENT, EI_PAD
	assert_eq!(header.e_ident, [127, 69, 76, 70, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
	assert_eq!(header.e_type, ET_DYN);
	assert_eq!(header.e_machine, EM_X86_64);
	assert_eq!(header.e_version, EV_CURRENT as _);

	let entry = header.e_entry;

	assert_eq!(header.e_ehsize as usize, size_of::<Header>());
	assert_eq!(header.e_phentsize as usize, size_of::<ProgramHeader>());
	assert_eq!(header.e_shentsize as usize, size_of::<SectionHeader>());

	assert_ne!(header.e_phnum, 0xFFFF); // case not supported
	let pheads = unsafe {
		slice::from_raw_parts(&binary[header.e_phoff as usize] as *const _ as *const ProgramHeader, header.e_phnum as _)
	};

	assert_ne!(header.e_shnum, 0); // case not supported
	let sheads = unsafe {
		slice::from_raw_parts(&binary[header.e_shoff as usize] as *const _ as *const SectionHeader, header.e_shnum as _)
	};

	assert_ne!(header.e_shstrndx as u32, SHN_UNDEF);
	let shstrndx = header.e_shstrndx;

	let dst = unsafe { slice::from_raw_parts_mut(ELF_START as *mut u8, ELF_SIZE as _) };

	let mut addr_high = ELF_START;
	let mut alloc_to = |addr: usize| {
		while ELF_START + addr >= addr_high {
			let page = Page::containing_address(VirtAddr::new(addr_high as _));
			let frame = frame_allocator.allocate_frame().ok_or(MapToError::FrameAllocationFailed).unwrap();
			let flags = PageTableFlags::PRESENT | PageTableFlags::WRITABLE;
			unsafe { mapper.map_to(page, frame, flags, frame_allocator).unwrap().flush() };
			addr_high += frame.size() as usize;
		}
	};

	let mut load = |src_off: usize, dst_off: usize, len: usize| {
		alloc_to(dst_off + len);
		dst[dst_off..dst_off + len].copy_from_slice(&binary[src_off..src_off + len]);
	};

	let mut dyns = None;

	for phead in pheads {
		match phead.p_type {
			PT_LOAD => load(phead.p_offset as usize, phead.p_vaddr as usize, phead.p_filesz as usize),
			PT_INTERP => {
				let interp = unsafe {
					str::from_utf8_unchecked(slice::from_raw_parts(
						&binary[phead.p_offset as usize],
						(phead.p_filesz - 1) as _,
					))
				};
				assert_eq!(interp, "/lib64/ld-linux-x86-64.so.2");
			},
			PT_PHDR => load(phead.p_offset as usize, phead.p_vaddr as usize, phead.p_filesz as usize),
			PT_DYNAMIC => {
				dyns = Some(unsafe {
					slice::from_raw_parts(
						&binary[phead.p_offset as usize] as *const _ as *const Dyn,
						phead.p_filesz as usize / size_of::<Dyn>(),
					)
				});
			},
			PT_NOTE => {
				let offset = phead.p_offset as usize;
				let end = offset + phead.p_filesz as usize;
				let mut cur = offset;
				while cur < end {
					let nhdr: &Nhdr32 = unsafe { &*(&binary[cur] as *const _ as *const _) };
					cur += size_of::<Nhdr32>();

					let namesz = nhdr.n_namesz as usize - 1;
					let name = unsafe { str::from_utf8_unchecked(&binary[cur..cur + namesz]) };
					cur = round_up(cur + namesz, 4);

					let descsz = nhdr.n_descsz as usize;
					let _desc = &binary[cur..cur + descsz];
					cur = round_up(cur + descsz, 4);

					println!("note {}", name);
				}
			},
			PT_TLS => println!("wtf is PT_TLS"),
			PT_GNU_EH_FRAME => println!("wtf is PT_GNU_EH_FRAME"),
			PT_GNU_STACK => println!("wtf is PT_GNU_STACK"),
			PT_GNU_RELRO => println!("wtf is PT_GNU_RELRO"),
			// PT_LOOS..=PT_HIOS => println!("ignoring PT_*OS"),
			_ => panic!("unrecognized type 0x{:x}", phead.p_type),
		}
	}

	let snames_head = &sheads[shstrndx as usize];
	let snames_slice = unsafe {
		slice::from_raw_parts(&binary[snames_head.sh_offset as usize] as *const _, snames_head.sh_size as usize)
	};

	for shead in sheads {
		if shead.sh_addr != 0 {
			let len = if shead.sh_type == SHT_NOBITS { 0 } else { shead.sh_size as usize };
			load(shead.sh_offset as usize, shead.sh_addr as usize, len);
		}
	}

	let mut dynstr = None;

	for shead in sheads {
		let name = shead.sh_name as usize;
		let len = unsafe { strlen(&snames_slice[name] as *const _) };
		let name = unsafe { str::from_utf8_unchecked(&snames_slice[name..name + len]) };

		let offset = shead.sh_offset as usize;
		let size = shead.sh_size as usize;

		match shead.sh_type {
			SHT_NULL => (),
			SHT_PROGBITS => (),
			SHT_SYMTAB => println!("{}, ignoring SHT_SYMTAB", name),
			SHT_STRTAB => match name {
				".dynstr" => {
					dynstr = Some(&binary[offset..offset + size]);
				},
				_ => println!("{}, ignoring SHT_STRTAB", name),
			},
			SHT_RELA => println!("{}, ignoring SHT_RELA", name),
			SHT_DYNAMIC => {
				dyns = Some(unsafe {
					slice::from_raw_parts(&binary[offset] as *const _ as *const Dyn, size / size_of::<Dyn>())
				});
			},
			SHT_NOTE => println!("{}, ignoring SHT_NOTE", name),
			SHT_NOBITS => (),
			SHT_DYNSYM => println!("{}, ignoring SHT_DYNSYM", name),
			SHT_INIT_ARRAY => println!("{}, ignoring SHT_INIT_ARRAY", name),
			SHT_FINI_ARRAY => println!("{}, ignoring SHT_FINI_ARRAY", name),
			SHT_GNU_HASH => println!("{}, wtf is SHT_GNU_HASH", name),
			SHT_GNU_VERNEED => println!("{}, wtf is SHT_GNU_VERNEED", name),
			SHT_GNU_VERSYM => println!("{}, wtf is SHT_GNU_VERSYM", name),
			_ => panic!("unrecognized section header type 0x{:x}", shead.sh_type),
		}
	}

	let dyns = dyns.unwrap();
	let dynstr = dynstr.unwrap();
	let mut init = None;
	let mut fini = None;

	for (i, dyna) in dyns.iter().enumerate() {
		match dyna.d_tag {
			DT_NULL => break,
			DT_NEEDED => {
				let val = dyna.d_val as usize;
				let len = unsafe { strlen(&dynstr[val] as *const _) };
				let name = unsafe { str::from_utf8_unchecked(&dynstr[val..val + len]) };
				println!("dylib {}", name)
			},
			DT_PLTRELSZ => println!("wtf is DT_PLTRELSZ"),
			DT_PLTGOT => println!("wtf is DT_PLTGOT"),
			DT_STRTAB => println!("ignoring DT_STRTAB"),
			DT_SYMTAB => println!("ignoring DT_SYMTAB"),
			DT_RELA => println!("ignoring DT_RELA"),
			DT_RELASZ => println!("ignoring DT_RELASZ"),
			DT_RELAENT => println!("ignoring DT_RELAENT"),
			DT_STRSZ => println!("ignoring DT_STRSZ"),
			DT_SYMENT => println!("ignoring DT_SYMENT"),
			DT_INIT => {
				assert!(init.is_none());
				init = Some(dyna.d_val);
			},
			DT_FINI => {
				assert!(fini.is_none());
				fini = Some(dyna.d_val);
			},
			DT_PLTREL => println!("ignoring DT_PLTREL"),
			DT_DEBUG => println!("ignoring DT_DEBUG"),
			DT_JMPREL => println!("ignoring DT_JMPREL"),
			DT_BIND_NOW => println!("ignoring DT_BIND_NOW"),
			DT_INIT_ARRAY => println!("wtf is DT_INIT_ARRAY"),
			DT_INIT_ARRAYSZ => println!("wtf is DT_INIT_ARRAYSZ"),
			DT_FINI_ARRAY => println!("wtf is DT_FINI_ARRAY"),
			DT_FINI_ARRAYSZ => println!("wtf is DT_FINI_ARRAYSZ"),
			DT_GNU_HASH => println!("wtf is DT_GNU_HASH"),
			DT_VERSYM => println!("wtf is DT_VERSYM"),
			DT_RELACOUNT => println!("wtf is DT_RELACOUNT"),
			DT_FLAGS_1 => println!("wtf is DT_FLAGS_1"),
			DT_VERNEED => println!("wtf is DT_VERNEED"),
			DT_VERNEEDNUM => println!("wtf is DT_VERNEEDNUM"),
			_ => panic!("unrecognized dyn tag 0x{:x}", dyna.d_tag),
		}
	}
	println!("{:?}", pheads[0].p_type);
}

unsafe fn strlen(st: *const u8) -> usize {
	let mut cur = st;
	while *cur != 0 {
		cur = cur.add(1);
	}
	cur as usize - st as usize
}

fn round_up(n: usize, align: usize) -> usize {
	let sub1 = align - 1;
	(n + sub1) & !sub1
}

unsafe fn jmp(addr: usize) {
	asm!("jmp *($0)" : : "r" (addr));
}
