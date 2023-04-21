use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::targets::{InitializationConfig, Target, TargetMachine};
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

fn main() -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let module = context.create_module("example");
    let builder = context.create_builder();

    let i32_type = context.i32_type();
    let fn_type = i32_type.fn_type(&[], false);

    let function = module.add_function("main", fn_type, None);
    let basic_block = context.append_basic_block(function, "entry");

    builder.position_at_end(basic_block);
    builder.build_return(Some(&context.i32_type().const_int(0, false)));

    Target::initialize_native(&InitializationConfig::default())?;
    let target_triple = TargetMachine::get_default_triple();

    let target = Target::from_triple(&target_triple).unwrap();

    let machine = target
        .create_target_machine(
            &target_triple,
            "",
            "",
            OptimizationLevel::Aggressive,
            inkwell::targets::RelocMode::Static,
            inkwell::targets::CodeModel::Small,
        )
        .ok_or("Error creating target machine")?;

    println!("Target triple: {}", target_triple);

    machine.write_to_file(
        &module,
        inkwell::targets::FileType::Object,
        Path::new("example.o"),
    )?;

    let pass_manager_builder = inkwell::passes::PassManagerBuilder::create();
    pass_manager_builder.set_optimization_level(OptimizationLevel::Aggressive);
    let pass_manager = inkwell::passes::PassManager::create(());

    machine.add_analysis_passes(&pass_manager);

    pass_manager.run_on(&module);

    module.write_bitcode_to_path(Path::new("example.bc"));

    Ok(())
}
