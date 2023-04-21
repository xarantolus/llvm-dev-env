mod compiler;

use inkwell::context::Context;

use llvm_test::compiler::{compile_options::CompileOptions, target::CompileTarget};
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let args = CompileOptions::parse_args()?;

    let context = Context::create();
    let module = context.create_module("example");
    let builder = context.create_builder();

    let i32_type = context.i32_type();
    let fn_type = i32_type.fn_type(&[], false);
    let function = module.add_function("main", fn_type, None);
    let basic_block = context.append_basic_block(function, "entry");
    builder.position_at_end(basic_block);
    builder.build_return(Some(&context.i32_type().const_int(0, false)));

    let target = CompileTarget::new(args);
    let machine = target
        .native_machine()
        .map_err(|e| format!("Error getting target machine: {}", e))?;

    target
        .generate_main_call(&machine, &module)
        .map_err(|e| format!("Error generating runtime call: {}", e))?;

    target
        .write_output(&machine, &module)
        .map_err(|e| format!("Error writing output: {}", e))?;

    Ok(())
}
