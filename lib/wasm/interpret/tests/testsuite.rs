use serde_derive::Deserialize;
use std::{
	env::set_current_dir,
	ffi::OsStr,
	fs::{self, create_dir_all, File},
	io::prelude::*,
	path::PathBuf,
	process::Command,
};
use wasm_core::{
	exec::runtime::{ModuleInst, Store, Val},
	syntax::modules::Module,
};
use wasm_interpret::Interpreter;

#[test]
fn i32() {
	set_current_dir("../../..").unwrap();
	create_dir_all("build/wasm/testsuite").unwrap();

	for entry in fs::read_dir("lib/wasm/testsuite").unwrap() {
		let entry = entry.unwrap();
		let path = entry.path();
		if !path.is_dir() && path.extension() == Some(OsStr::new("wast")) {
			run_wast(path);
		}
	}

	println!("success");
}

fn run_wast(path: PathBuf) {
	let build_dir = "build/wasm/testsuite";
	let json = format!("{}/{}.json", build_dir, path.file_stem().unwrap().to_str().unwrap());

	Command::new("wast2json").args(&[path.to_str().unwrap(), "-o", &json]).output().unwrap();
	println!("{}", json);
	let tests: Tests = serde_json::from_reader(File::open(json).unwrap()).unwrap();

	let mut store = None;
	let mut inst = None;
	for cmd in tests.commands {
		if let Some(filename) = cmd.filename.as_ref() {
			if filename.ends_with(".wat") {
				continue;
			}
		}

		match &*cmd.typ {
			"module" => {
				let mut wasm = vec![];
				let mut file = File::open(format!("{}/{}", build_dir, cmd.filename.unwrap())).unwrap();
				file.read_to_end(&mut wasm).unwrap();
				let mut module = Module::decode(&wasm).unwrap();
				store = Some(Store::<Interpreter>::init());
				inst = Some(module.instantiate(store.as_mut().unwrap(), &[]).unwrap());
			},
			"assert_return" => {
				let action = cmd.action.unwrap();
				match &*action.typ {
					"invoke" => {
						let results = invoke(store.as_mut().unwrap(), inst.as_ref().unwrap(), action).unwrap();
						for (i, expect) in cmd.expected.unwrap().into_iter().enumerate() {
							let value = expect.value.unwrap();
							let (val, eq) = match &*expect.typ {
								"i32" => {
									let val = Val::I32(value.parse::<u32>().unwrap() as _);
									(val, val == results[i])
								},
								"i64" => {
									let val = Val::I64(value.parse::<u64>().unwrap() as _);
									(val, val == results[i])
								},
								"f32" => {
									let val = Val::F32(value.parse().unwrap());
									(val, val == results[i])
								},
								_ => unimplemented!("{}", expect.typ),
							};
							assert!(
								eq,
								"failed test at {}:{} (expected {:?} == {:?})",
								tests.source_filename, cmd.line, val, results[i]
							);
						}
					},
					_ => unimplemented!("{}", action.typ),
				}
			},
			"assert_trap" => {
				let action = cmd.action.unwrap();
				match &*action.typ {
					"invoke" => {
						invoke(store.as_mut().unwrap(), inst.as_ref().unwrap(), action).expect_err(&format!(
							"failed test at {}:{} (expected trap)",
							tests.source_filename, cmd.line
						));
					},
					_ => unimplemented!("{}", action.typ),
				}
			},
			"assert_invalid" => {
				let mut wasm = vec![];
				let mut file = File::open(format!("{}/{}", build_dir, cmd.filename.unwrap())).unwrap();
				file.read_to_end(&mut wasm).unwrap();
				let mut module = Module::decode(&wasm).unwrap();
				module
					.validate()
					.expect_err(&format!("failed test at {}:{} (expected invalid)", tests.source_filename, cmd.line));
			},
			_ => unimplemented!("{}", cmd.typ),
		}
	}
}

fn invoke(
	store: &mut Store<Interpreter>,
	inst: &ModuleInst<Interpreter>,
	action: Action,
) -> Result<Vec<Val>, &'static str> {
	let inst = inst;
	let func = inst.exports.get(&action.field).unwrap().as_func();
	let mut args = vec![];
	for arg in action.args {
		let value = arg.value.unwrap();
		args.push(match &*arg.typ {
			"i32" => Val::I32(value.parse::<u32>().unwrap() as _),
			_ => unimplemented!("{}", arg.typ),
		});
	}
	println!("{} {:?}", action.field, args);
	store.invoke(&inst, func, args)
}

#[derive(Deserialize)]
struct Tests {
	source_filename: String,
	commands: Vec<TestCommand>,
}

#[derive(Deserialize)]
struct TestCommand {
	#[serde(rename = "type")]
	typ: String,
	line: i16,
	filename: Option<String>,
	action: Option<Action>,
	expected: Option<Vec<TestVal>>,
}

#[derive(Deserialize)]
struct Action {
	#[serde(rename = "type")]
	typ: String,
	field: String,
	args: Vec<TestVal>,
}

#[derive(Deserialize)]
struct TestVal {
	#[serde(rename = "type")]
	typ: String,
	value: Option<String>,
}
