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

use crate::diagnostics::DiagnosticLevel;

#[derive(Debug)]
pub struct Settings<'args> {
    // warning options
    pub warn_duplicate_file: DiagnosticLevel,
    // other settings
    pub setting_architecture: &'args str,
    pub setting_output_prefix: &'args str,
    // flags
    pub flag_validate_ir: bool,
}
impl<'args> Default for Settings<'args> {
    fn default() -> Self {
        Self {
            warn_duplicate_file: DiagnosticLevel::Error,
            setting_architecture: concat!(
                env!("CARGO_CFG_TARGET_ARCH"),
                "-",
                env!("CARGO_CFG_TARGET_OS"),
            ),
            setting_output_prefix: "",
            flag_validate_ir: false,
        }
    }
}
