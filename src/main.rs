// Copyright 2024 Justin Hu
//
// This file is part of the Half-Zero Compiler.
//
// The Half-Zero Compiler is free software: you can redistribute it and/or
// modify it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or (at your
// option) any later version.
//
// The Half-Zero Compiler is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// the Half-Zero Compiler. If not, see <https://www.gnu.org/licenses/>.
//
// SPDX-License-Identifier: GPL-3.0-or-later

#![forbid(unsafe_code)]

pub mod arg_parse;
pub mod backend;
pub mod diagnostics;
pub mod frontend;
pub mod middleend;
pub mod settings;
pub mod source_location;

use std::{env::args_os, fs::File, io::Write, path::Path, process::ExitCode};

use arg_parse::{parse_args, Configuration};
use diagnostics::internal_error;
use frontend::{parser::parse, type_check::type_check};
use middleend::c_level_ir::{borrow_check::borrow_check, translator::to_c_level_ir};

use crate::{
    backend::{x86_64_linux, FileContents},
    diagnostics::{diagnostic, DiagnosticLevel},
    middleend::low_level_ir::translator::to_low_level_ir,
    source_location::SourceLocation,
};

fn print_usage() {
    println!("Usage: h0c [option...] [--] <file...>");
    print!(
        r"For more information, see the 'README.md' file.

Options:
  --help, -h, -?    Display this information and exit
  --version         Display version information and exit
  --arch=...        Set the target architecture
  -W...=...         Configure warning options
  --debug-dump=...  Configure debug information

Please report bugs at <https://github.com/JustinHuPrime/HalfZero/issues>
"
    );
}

fn print_version() {
    println!(
        "Half-Zero Compiler (h0c) version {}",
        env!("CARGO_PKG_VERSION")
    );
    print!(
        r"Copyright 2024 Justin Hu
This is free software; see the source for copying conditions. There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
"
    );
}

pub fn main() -> ExitCode {
    // preliminary args stuff
    let args = args_os().collect::<Vec<_>>();
    let args = match args
        .into_iter()
        .map(|arg| arg.into_string())
        .collect::<Result<Vec<_>, _>>()
    {
        Err(_) => {
            eprintln!("error: expected valid UTF-8 only in command line arguments");
            return ExitCode::FAILURE;
        }
        Ok(args) => args,
    };
    let args = if !args.is_empty() {
        args[1..].iter().map(|arg| arg.as_str()).collect::<Vec<_>>()
    } else {
        Vec::new()
    };

    if args
        .iter()
        .any(|arg| matches!(*arg, "--help" | "-h" | "-?"))
    {
        print_usage();
        return ExitCode::SUCCESS;
    }
    if args.iter().any(|arg| matches!(*arg, "--version")) {
        print_version();
        return ExitCode::SUCCESS;
    }

    // parse args
    let (
        Configuration {
            interface_files,
            implementation_files,
            settings,
        },
        mut report,
    ) = match parse_args(args) {
        Ok((config, report)) => (config, report),
        Err(report) => {
            eprint!("{}", report);
            return ExitCode::FAILURE;
        }
    };

    // frontend (interfaces)
    // TODO: parallelize this
    let mut interface_asts = interface_files
        .iter()
        .filter_map(|filename| match parse(filename) {
            Ok((ast, file_report)) => {
                report.append(file_report);
                Some(ast)
            }
            Err(file_report) => {
                report.append(file_report);
                None
            }
        })
        .collect::<Vec<_>>();
    if report.has_errors() {
        eprint!("{}", report);
        return ExitCode::FAILURE;
    }
    for interface_ast in interface_asts.iter_mut() {
        report.append(type_check(interface_ast));
    }
    if report.has_errors() {
        eprint!("{}", report);
        return ExitCode::FAILURE;
    }

    // TODO: parallelize this
    for implementation_file in implementation_files {
        // frontend

        let mut ast = match parse(implementation_file) {
            Ok((ast, file_report)) => {
                report.append(file_report);
                ast
            }
            Err(file_report) => {
                report.append(file_report);
                continue;
            }
        };

        let file_report = type_check(&mut ast);
        if file_report.has_errors() {
            report.append(file_report);
            continue;
        }
        report.append(file_report);

        // middleend

        let mut c_level_ir = to_c_level_ir(&ast, &interface_asts);

        let file_report = borrow_check(&mut c_level_ir);
        if file_report.has_errors() {
            report.append(file_report);
            continue;
        }
        report.append(file_report);

        // TODO: C-level IR optimizations

        let mut low_level_ir = to_low_level_ir(&c_level_ir);

        // TODO: low-level IR optimizations

        // backend

        let FileContents { filename, contents } = match settings.setting_architecture {
            "x86_64-linux" => x86_64_linux::backend(&low_level_ir, &settings),
            _ => internal_error!(
                "invalid architecture '{}' encountered",
                settings.setting_architecture
            ),
        };

        let mut file = match File::create(Path::new(&filename)) {
            Ok(file) => file,
            Err(err) => {
                report.push(diagnostic!(
                    DiagnosticLevel::Error,
                    SourceLocation::in_file(implementation_file),
                    "could not output to file '{filename}': {err}"
                ));
                continue;
            }
        };
        if let Err(err) = file.write_all(contents.as_bytes()) {
            report.push(diagnostic!(
                DiagnosticLevel::Error,
                SourceLocation::in_file(implementation_file),
                "could not output to file '{filename}': {err}"
            ));
            continue;
        }
    }

    eprint!("{}", report);
    if report.has_errors() {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}
