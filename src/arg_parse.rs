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

use crate::{
    diagnostics::{
        diagnostic, locationless_diagnostic, DiagnosticLevel, DiagnosticReport, DiagnosticResult,
    },
    settings::Settings,
    source_location::SourceLocation,
};

#[derive(Debug)]
pub struct Configuration<'args> {
    pub interface_files: Vec<&'args str>,
    pub implementation_files: Vec<&'args str>,
    pub settings: Settings<'args>,
}

pub fn parse_args(args: Vec<&str>) -> DiagnosticResult<Configuration> {
    let mut report = DiagnosticReport::default();
    let mut interface_files = Vec::new();
    let mut implementation_files = Vec::new();
    let mut setting_args = Vec::new();

    // parse regular args
    let mut all_files = false;
    for arg in args {
        if arg == "--" {
            all_files = true;
        } else if arg.starts_with('-') && !all_files {
            setting_args.push(arg)
        } else if arg.ends_with(".h0i") {
            interface_files.push(arg);
        } else if arg.ends_with(".h0") {
            implementation_files.push(arg);
        } else {
            report.push(diagnostic!(
                DiagnosticLevel::Error,
                SourceLocation::in_file(arg),
                "file extension not recognized",
            ));
        }
    }

    if implementation_files.is_empty() {
        report.push(locationless_diagnostic!(
            DiagnosticLevel::Error,
            "no implementation files given",
        ));
    }

    // parse settings
    let mut settings = Settings::default();

    for setting in setting_args {
        if setting.starts_with("-W") {
            let (_, setting) = setting.split_at(2);
            let split_index = match setting.find('=') {
                Some(index) => index,
                None => {
                    report.push(locationless_diagnostic!(
                        DiagnosticLevel::Error,
                        "could not parse warning '{setting}'"
                    ));
                    continue;
                }
            };
            let (name, level) = setting.split_at(split_index);
            let (_, level) = level.split_at(1);

            let name = match name {
                "duplicate-file" => &mut settings.warn_duplicate_file,
                name => {
                    report.push(locationless_diagnostic!(
                        DiagnosticLevel::Error,
                        "unrecognized warning '{name}'"
                    ));
                    continue;
                }
            };

            let level = match level {
                "ignore" => DiagnosticLevel::Ignore,
                "warn" => DiagnosticLevel::Warn,
                "error" => DiagnosticLevel::Error,
                level => {
                    report.push(locationless_diagnostic!(
                        DiagnosticLevel::Error,
                        "unrecognized warning level '{level}'"
                    ));
                    continue;
                }
            };

            *name = level;
        } else if setting.starts_with("--") {
            let (_, setting) = setting.split_at(2);
            let split_index = match setting.find('=') {
                Some(index) => index,
                None => {
                    report.push(locationless_diagnostic!(
                        DiagnosticLevel::Error,
                        "could not parse setting '{setting}'"
                    ));
                    continue;
                }
            };
            let (name, value) = setting.split_at(split_index);
            let (_, value) = value.split_at(1);

            match name {
                "architecture" => match value {
                    "x86_64-linux" => {
                        settings.setting_architecture = value;
                    }
                    value => report.push(locationless_diagnostic!(
                        DiagnosticLevel::Error,
                        "unsupported architecture '{value}'"
                    )),
                },
                name => {
                    report.push(locationless_diagnostic!(
                        DiagnosticLevel::Error,
                        "unrecognized setting '{name}'"
                    ));
                    continue;
                }
            };
        } else {
            // starts with '-'

            let (_, setting) = setting.split_at(1);
            let (name, value) = if setting.starts_with("no-") {
                (setting.split_at(3).1, false)
            } else {
                (setting, true)
            };

            let name = match name {
                "validate-ir" => &mut settings.flag_validate_ir,
                name => {
                    report.push(locationless_diagnostic!(
                        DiagnosticLevel::Error,
                        "unrecognized flag '{name}'"
                    ));
                    continue;
                }
            };
            *name = value;
        }
    }

    if report.has_errors() {
        return Err(report);
    }

    // check files for duplicates
    interface_files.sort();
    implementation_files.sort();
    check_duplicates(&interface_files, &settings, &mut report);
    check_duplicates(&implementation_files, &settings, &mut report);
    interface_files.dedup();
    implementation_files.dedup();

    if report.has_errors() {
        Err(report)
    } else {
        Ok((
            Configuration {
                interface_files,
                implementation_files,
                settings,
            },
            report,
        ))
    }
}

fn check_duplicates<'args>(
    files: &[&'args str],
    settings: &Settings,
    report: &mut DiagnosticReport<'args>,
) {
    let mut idx = 0;
    while idx < files.len() {
        if idx != 0 && files[idx - 1] == files[idx] {
            report.push(diagnostic!(
                settings.warn_duplicate_file,
                SourceLocation::in_file(files[idx]),
                "file specified more than once"
            ));
            while idx < files.len() && files[idx - 1] == files[idx] {
                idx += 1;
            }
            continue;
        }
        idx += 1;
    }

    // for lhs in 0..files.len() {
    //     for rhs in (lhs + 1)..files.len() {
    //         if files[lhs] == files[rhs] {
    //             report.push(diagnostic!(
    //                 settings.warn_duplicate_file,
    //                 SourceLocation::in_file(files[lhs]),
    //                 "file specified more than once"
    //             ));
    //         }
    //     }
    // }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bad_extensions() {
        parse_args(vec!["bad.extension", "implementation.h0"])
            .expect_err("should have bad extension error");
    }

    #[test]
    fn test_no_impl_file() {
        parse_args(vec!["interface.h0i"]).expect_err("should have no impl file error");
    }

    #[test]
    fn test_malformed_warning() {
        parse_args(vec!["-Wmalformed", "impl.h0"]).expect_err("should have bad warning error");
    }

    #[test]
    fn test_invalid_warning() {
        parse_args(vec!["-Wnotawarning=ignore", "impl.h0"])
            .expect_err("should have bad warning error");
    }

    #[test]
    fn test_invalid_warning_level() {
        parse_args(vec!["-Wduplicate-file=notalevel", "impl.h0"])
            .expect_err("should have bad warning level error");
    }

    #[test]
    fn test_malformed_setting() {
        parse_args(vec!["--thing", "impl.h0"]).expect_err("should have bad setting error");
    }

    #[test]
    fn test_invalid_setting() {
        parse_args(vec!["--notasetting=thing", "impl.h0"])
            .expect_err("should have bad setting error");
    }

    #[test]
    fn test_invalid_arch() {
        parse_args(vec!["--arch=doesntexist", "impl.h0"]).expect_err("should have bad arch error");
    }

    #[test]
    fn test_duplicate_files() {
        parse_args(vec!["-Wduplicate-file=error", "impl.h0", "impl.h0"])
            .expect_err("should have duplicate file error");
        parse_args(vec!["-Wduplicate-file=error", "interf.h0i", "interf.h0i"])
            .expect_err("should have duplicate file error");
        parse_args(vec![
            "-Wduplicate-file=error",
            "impl.h0",
            "interf.h0i",
            "impl.h0",
        ])
        .expect_err("should have duplicate file error");
        parse_args(vec![
            "-Wduplicate-file=error",
            "impl.h0",
            "interf.h0i",
            "impl.h0",
            "impl.h0",
        ])
        .expect_err("should have duplicate file error");
    }

    #[test]
    fn test_interface_files() {
        assert_eq!(
            parse_args(vec!["first.h0i", "impl.h0", "second.h0i"])
                .unwrap()
                .0
                .interface_files,
            vec!["first.h0i", "second.h0i"]
        );
        assert_eq!(
            parse_args(vec!["first.h0i", "impl.h0", "--", "-second.h0i"])
                .unwrap()
                .0
                .interface_files,
            vec!["-second.h0i", "first.h0i"]
        );
    }

    #[test]
    fn test_impl_files() {
        assert_eq!(
            parse_args(vec!["first.h0", "interf.h0i", "second.h0"])
                .unwrap()
                .0
                .implementation_files,
            vec!["first.h0", "second.h0"]
        );
        assert_eq!(
            parse_args(vec!["first.h0", "interf.h0i", "--", "-second.h0"])
                .unwrap()
                .0
                .implementation_files,
            vec!["-second.h0", "first.h0"]
        );
    }

    #[test]
    fn test_settings() {
        assert_eq!(
            parse_args(vec!["-Wduplicate-file=ignore", "thing.h0", "thing.h0"])
                .unwrap()
                .0
                .implementation_files,
            vec!["thing.h0"]
        );

        assert!(
            parse_args(vec!["thing.h0", "-validate-ir"])
                .unwrap()
                .0
                .settings
                .flag_validate_ir
        );
    }
}
