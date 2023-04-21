use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::targets::{InitializationConfig, Target, TargetMachine};
use inkwell::types::{BasicType, FunctionType};
use inkwell::OptimizationLevel;
use std::error::Error;
use std::path::Path;

/// Convenience type alias for the `sum` function.
///
/// Calling this is innately `unsafe` because there's no guarantee it doesn't
/// do `unsafe` operations internally.
type SumFunc = unsafe extern "C" fn(u64, u64, u64) -> u64;

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
}

fn add_runtime(module: &Module, machine: &TargetMachine) -> Result<(), Box<dyn Error>> {
    let main_fnt = module
        .get_function("main")
        .ok_or("No main function defined")?;

    // Make sure main_fnt returns any integer type
    let main_fnt_type = main_fnt.get_type();
    let ret_type = main_fnt_type
        .get_return_type()
        .ok_or("main function must return a value")?;
    if !ret_type.is_int_type() {
        return Err("main function must return an integer".into());
    }

    // Switch depending on target OS and architecture
    let target_triple = machine.get_triple();
    let tt = target_triple.as_str().to_string_lossy();

    if tt.contains("linux") && tt.contains("x86_64") {

        module.set_inline_assembly(
            r#"
        .intel_syntax noprefix
        .section .text
        .globl _start
        _start:
            call main
            mov rdi, rax
            mov rax, 60
            syscall
        "#,
        );
    } else {
        return Err(format!(
            "Unsupported target: {}",
            target_triple.as_str().to_string_lossy()
        )
        .into());
    }

    Ok(())
}

fn native_machine() -> Result<TargetMachine, Box<dyn Error>> {
    Target::initialize_native(&InitializationConfig::default())?;
    let target = Target::from_triple(&TargetMachine::get_default_triple())?;
    let machine = target
        .create_target_machine(
            &TargetMachine::get_default_triple(),
            "generic",
            "",
            OptimizationLevel::Default,
            inkwell::targets::RelocMode::Static,
            inkwell::targets::CodeModel::Small,
        )
        .ok_or("Could not create target machine")?;
    Ok(machine)
}

fn main() -> Result<(), Box<dyn Error>> {
    let machine = native_machine()?;

    let context = Context::create();
    let module = context.create_module("example");
    let builder = context.create_builder();

    let i32_type = context.i32_type();
    let fn_type = i32_type.fn_type(&[], false);
    let function = module.add_function("main", fn_type, None);
    let basic_block = context.append_basic_block(function, "entry");
    builder.position_at_end(basic_block);
    builder.build_return(Some(&context.i32_type().const_int(1, false)));

    add_runtime(&module, &machine)?;

    let pass_manager_builder = inkwell::passes::PassManagerBuilder::create();
    pass_manager_builder.set_optimization_level(OptimizationLevel::Aggressive);
    let pass_manager = inkwell::passes::PassManager::create(());

    machine.add_analysis_passes(&pass_manager);

    pass_manager.run_on(&module);

    machine.write_to_file(
        &module,
        inkwell::targets::FileType::Object,
        Path::new("example.o"),
    )?;

    // ld -nostdlib -nodefaultlibs -e main example.o -o output
    std::process::Command::new("ld")
        .args(&[
            "-nostdlib",
            "-nodefaultlibs",
            "-e",
            "_start",
            "example.o",
            "-o",
            "output",
        ])
        .output()?;

    Ok(())
}
