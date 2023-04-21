use std::{error::Error, path::Path};

use clap::{arg, command, Arg, ArgAction};
use inkwell::{targets::FileType, OptimizationLevel};

#[derive(Clone, Debug)]
pub struct CompileInfo {
    input_files: Vec<Box<Path>>,

    target_file: Box<Path>,

    optimization_level: OptimizationLevel,

    output_format: FileType,
}

impl CompileInfo {
    pub fn parse_args() -> Result<CompileInfo, Box<dyn Error>> {
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

        let out_file_path = match matches.get_one::<String>("output") {
            None => "a.out",
            Some(s) => s.as_str(),
        };

        Ok(CompileInfo {
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

            output_format: if matches.get_flag("assembly") {
                FileType::Assembly
            } else {
                FileType::Object
            },
        })
    }
}
