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
    fmt::{self, Display, Formatter},
};

use crate::source_location::SourceLocation;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum DiagnosticLevel {
    Ignore,
    Warn,
    Error,
}
impl Display for DiagnosticLevel {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ignore => write!(f, "ignored"),
            Self::Warn => write!(f, "warning"),
            Self::Error => write!(f, "error"),
        }
    }
}

#[derive(Debug)]
pub struct Diagnostic<'args> {
    level: DiagnosticLevel,
    location: Option<SourceLocation<'args>>,
    message: String,
}
impl<'args> Diagnostic<'args> {
    pub fn new(
        level: DiagnosticLevel,
        location: Option<SourceLocation<'args>>,
        message: String,
    ) -> Self {
        Self {
            level,
            location,
            message,
        }
    }
}
impl<'args> Display for Diagnostic<'args> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.location {
            Some(location) => {
                write!(f, "{}: {}: {}", location, self.level, self.message)
            }
            None => {
                write!(f, "{}: {}", self.level, self.message)
            }
        }
    }
}
impl<'args> Ord for Diagnostic<'args> {
    fn cmp(&self, other: &Self) -> Ordering {
        match (&self.location, &other.location) {
            (None, None) => Ordering::Equal,
            (Some(_), None) => Ordering::Greater,
            (None, Some(_)) => Ordering::Less,
            (Some(lhs), Some(rhs)) => lhs.cmp(rhs),
        }
    }
}
impl<'args> PartialOrd for Diagnostic<'args> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl<'args> Eq for Diagnostic<'args> {}
impl<'args> PartialEq for Diagnostic<'args> {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

macro_rules! diagnostic {
    ($level:expr, $location:expr, $($message:tt)*) => {
        crate::diagnostics::Diagnostic::new($level, Some($location), format!($($message)*))
    };
}
pub(crate) use diagnostic;

macro_rules! locationless_diagnostic {
    ($level:expr, $($message:tt)*) => {
        crate::diagnostics::Diagnostic::new($level, None, format!($($message)*))
    };
}
pub(crate) use locationless_diagnostic;

#[derive(Debug, Default)]
pub struct DiagnosticReport<'args> {
    diagnostics: Vec<Diagnostic<'args>>,
}
impl<'args> DiagnosticReport<'args> {
    pub fn has_errors(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|diagnostic| diagnostic.level == DiagnosticLevel::Error)
    }
    pub fn push(&mut self, diagnostic: Diagnostic<'args>) {
        if diagnostic.level >= DiagnosticLevel::Warn {
            self.diagnostics.push(diagnostic);
        }
    }
    pub fn append(&mut self, mut report: DiagnosticReport<'args>) {
        self.diagnostics.append(&mut report.diagnostics);
    }
}
impl<'args> Display for DiagnosticReport<'args> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut diagnostics = self.diagnostics.iter().collect::<Vec<_>>();
        diagnostics.sort();

        for diagnostic in diagnostics {
            writeln!(f, "{}", diagnostic)?;
        }
        Ok(())
    }
}

pub type DiagnosticResult<'args, T> = Result<(T, DiagnosticReport<'args>), DiagnosticReport<'args>>;

macro_rules! internal_error {
    ($($message:tt)+) => {
        panic!("internal compiler error: {}:{}: {}\nPlease report this at <https://github.com/JustinHuPrime/HalfZero/issues>", file!(), line!(), format!($($message)*))
    };
}
pub(crate) use internal_error;
