use inkwell::targets::{FileType, InitializationConfig, Target, TargetMachine};
use std::{error::Error, path::Path};

use inkwell::module::Module;

use super::compile_options::CompileOptions;

pub struct CompileTarget {
    options: CompileOptions,
}

impl CompileTarget {
    pub fn new(options: CompileOptions) -> CompileTarget {
        CompileTarget { options }
    }

    // Gets a TargetMachine with our options
    pub fn native_machine(&self) -> Result<TargetMachine, Box<dyn Error>> {
        Target::initialize_native(&InitializationConfig::default())?;
        let trip = TargetMachine::get_default_triple();
        let target = Target::from_triple(&trip)?;
        let machine = target
            .create_target_machine(
                &trip,
                "generic",
                "",
                self.options.optimization_level,
                inkwell::targets::RelocMode::Static,
                inkwell::targets::CodeModel::Small,
            )
            .ok_or("Could not create target machine")?;
        Ok(machine)
    }

    /// Add small setup code to the module that calls an already defined main function
    pub fn generate_main_call(
        &self,
        machine: &TargetMachine,
        module: &Module,
    ) -> Result<(), Box<dyn Error>> {
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

    pub fn write_output(
        &self,
        machine: &TargetMachine,
        module: &Module,
    ) -> Result<(), Box<dyn Error>> {
        if self.options.output_format == FileType::Assembly {
            machine.write_to_file(
                &module,
                self.options.output_format,
                &*self.options.target_file,
            )?;
        } else {
            // Create temporary directory
            let temp_dir = tempfile::tempdir()?;
            let intermediate_path = temp_dir.path().join("intermediate.o");

            // Generate output object file
            machine.write_to_file(&module, self.options.output_format, &intermediate_path)?;

            // Get absolute path of output file
            let output_abs = Path::new(&*self.options.target_file).canonicalize()?;

            // Link / Convert to ELF
            std::process::Command::new("ld")
                .args(&[
                    "-nostdlib",
                    "-nodefaultlibs",
                    "-e",
                    "_start",
                    intermediate_path
                        .to_str()
                        .expect("Invalid intermediate object file path"),
                    "-o",
                    output_abs.to_str().expect("Invalid output file path"),
                ])
                .current_dir(temp_dir.path())
                .output()?;
        }

        Ok(())
    }
}
