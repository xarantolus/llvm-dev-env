use std::{error::Error, path::Path};

use clap::{arg, command, Arg, ArgAction};
use inkwell::{targets::FileType, OptimizationLevel};

#[derive(Clone, Debug)]
pub struct CompileOptions {
    pub input_files: Vec<Box<Path>>,

    pub target_file: Box<Path>,

    pub optimization_level: OptimizationLevel,

    pub output_format: FileType,
}

impl CompileOptions {
    pub fn parse_args() -> Result<CompileOptions, Box<dyn Error>> {
        let matches = command!()
            .arg(
                arg!(
                        -o --output <output_file> "Output file"
                )
                .required(false),
            )
            .arg(
                arg!(
                        -O --opt <level> "Optimization level"
                )
                .required(false),
            )
            .arg(
                Arg::new("assembly")
                    .short('S')
                    .help("Output assembly instead of an executable")
                    .action(ArgAction::SetTrue)
                    .default_value("false"),
            )
            .arg(Arg::new("files").action(ArgAction::Append))
            .get_matches();

        let remaining_args: Vec<_> = matches
            .get_many::<String>("files")
            .unwrap_or_default()
            .collect();
        if remaining_args.is_empty() {
            return Err("No input files".into());
        }

        let assembly = matches.get_flag("assembly");

        let out_file_path = match matches.get_one::<String>("output") {
            None => {
                if assembly {
                    "a.S"
                } else {
                    "a.out"
                }
            }
            Some(s) => s.as_str(),
        };

        Ok(CompileOptions {
            input_files: remaining_args
                .iter()
                .map(|s| Box::from(Path::new(s)))
                .collect(),

            target_file: Box::from(Path::new(out_file_path)),

            optimization_level: match matches.get_one::<String>("opt") {
                None => OptimizationLevel::Default,
                Some(s) => match s.as_str() {
                    "0" => OptimizationLevel::None,
                    "1" => OptimizationLevel::Less,
                    "2" => OptimizationLevel::Default,
                    "3" => OptimizationLevel::Aggressive,
                    _ => panic!("Invalid optimization level"),
                },
            },

            output_format: if assembly {
                FileType::Assembly
            } else {
                FileType::Object
            },
        })
    }
}
