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

use crate::diagnostics::DiagnosticResult;

use super::ast::AstNode;

pub fn parse(filename: &str) -> DiagnosticResult<AstNode> {
    todo!();
}
