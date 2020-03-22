#![no_std]

pub type Errno = u16;
pub type Exitcode = u32;
pub type Fd = u32;
pub type Preopentype = u8;
pub type Size = usize;

pub struct Ciovec {
	pub buf: *const u8,
	pub buf_len: Size,
}

pub struct Prestat {
	pub pr_type: Preopentype,
	pub u: PrestatU,
}

#[derive(Clone, Copy)]
pub struct PrestatDir {
	pub pr_name_len: Size,
}

pub union PrestatU {
	pub dir: PrestatDir,
}

pub unsafe extern fn environ_get(environ: *mut *mut u8, environ_buf: *mut u8) -> Errno {
	panic!("environ_get({:p}, {:p})", environ, environ_buf);
}

pub unsafe extern fn environ_sizes_get(argc: *mut Size, argv_buf_size: *mut Size) -> Errno {
	*argc = 0;
	*argv_buf_size = 0;
	0
}

pub unsafe extern fn fd_prestat_get(fd: Fd, _buf: *mut Prestat) -> Errno {
	if fd > 2 { 8 } else { unimplemented!() }
}

pub unsafe extern fn fd_prestat_dir_name(fd: Fd, path: *mut u8, path_len: Size) -> Errno {
	panic!("fd_prestat_dir_name({}, {:p}, {})", fd, path, path_len);
}

pub unsafe extern fn fd_write(fd: Fd, iovs_ptr: *const Ciovec, iovs_len: usize, nwritten: *mut Size) -> Errno {
	panic!("fd_write({}, {:p}, {}, {:p})", fd, iovs_ptr, iovs_len, nwritten);
}

pub unsafe extern fn proc_exit(rval: Exitcode) -> ! {
	panic!("proc_exit({})", rval);
}
