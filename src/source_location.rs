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

use std::{
    cmp::Ordering,
    fmt::{Display, Formatter, Result},
    num::NonZeroU64,
};

use crate::diagnostics::internal_error;

#[derive(Debug)]
pub struct SourceLocation<'args> {
    file: &'args str,
    start_line: Option<NonZeroU64>,
    start_column: Option<NonZeroU64>,
    end_line: Option<NonZeroU64>,
    end_column: Option<NonZeroU64>,
}
impl<'args> SourceLocation<'args> {
    pub fn in_file(file: &'args str) -> Self {
        Self {
            file,
            start_line: None,
            start_column: None,
            end_line: None,
            end_column: None,
        }
    }
    pub fn at_line(file: &'args str, start_line: NonZeroU64) -> Self {
        Self {
            file,
            start_line: Some(start_line),
            start_column: None,
            end_line: None,
            end_column: None,
        }
    }
    pub fn at_column(file: &'args str, start_line: NonZeroU64, start_column: NonZeroU64) -> Self {
        Self {
            file,
            start_line: Some(start_line),
            start_column: Some(start_column),
            end_line: None,
            end_column: None,
        }
    }
    pub fn of_lines(file: &'args str, start_line: NonZeroU64, end_line: NonZeroU64) -> Self {
        Self {
            file,
            start_line: Some(start_line),
            start_column: None,
            end_line: Some(end_line),
            end_column: None,
        }
    }
    pub fn of_columns(
        file: &'args str,
        start_line: NonZeroU64,
        start_column: NonZeroU64,
        end_line: NonZeroU64,
        end_column: NonZeroU64,
    ) -> Self {
        Self {
            file,
            start_line: Some(start_line),
            start_column: Some(start_column),
            end_line: Some(end_line),
            end_column: Some(end_column),
        }
    }
}
impl<'args> Display for SourceLocation<'args> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match (self.start_line, self.start_column) {
            (None, None) => write!(f, "{}", self.file),
            (Some(start_line), None) => write!(f, "{}:{}", self.file, start_line),
            (Some(start_line), Some(start_column)) => {
                write!(f, "{}:{}:{}", self.file, start_line, start_column)
            }
            _ => internal_error!(
                file!(),
                line!(),
                "source location may not have a column without a line"
            ),
        }
    }
}
impl<'args> Ord for SourceLocation<'args> {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.file, self.start_line, self.start_column).cmp(&(
            other.file,
            other.start_line,
            other.start_column,
        ))
    }
}
impl<'args> PartialOrd for SourceLocation<'args> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl<'args> Eq for SourceLocation<'args> {}
impl<'args> PartialEq for SourceLocation<'args> {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}
