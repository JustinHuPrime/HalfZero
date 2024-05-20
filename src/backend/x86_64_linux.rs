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
    num::NonZeroU64,
};

use crate::{diagnostics::internal_error, middleend::low_level_ir::LowLevelIR, settings::Settings};

use self::{allocator::allocate_temporary_locations, translator::to_temp_asm};

use super::FileContents;

pub mod allocator;
pub mod translator;

/// An assembly file, for one translation unit
#[derive(Debug)]
pub struct Asm<'args> {
    /// Translation unit's filename (e.g. something.h0)
    filename: &'args str,
    /// List of external symbols referenced by this file
    externs: Vec<String>,
    /// List of fragments in this file
    blocks: Vec<AsmFrag>,
}
impl<'args> Display for Asm<'args> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut blocks = self.blocks.iter().collect::<Vec<_>>();
        blocks.sort();

        if !self.externs.is_empty() {
            writeln!(f, "extern {}", self.externs.join(","))?;
        }
        let mut section_header = "";
        for block in blocks {
            if section_header != block.section_header() {
                section_header = block.section_header();
                writeln!(f, "{}", block.section_header())?;
            }
            write!(f, "{}", block)?;
        }
        writeln!(f, "section .comment")?;
        writeln!(f, "db \"Half-Zero version {}\"", env!("CARGO_PKG_VERSION"))?;
        writeln!(f, "section .note.GNU-stack noalloc noexec nowrite progbits")
    }
}

/// An assembly fragment - either function, some static data, or some
/// statically allocated space
#[derive(Debug)]
pub enum AsmFrag {
    /// A function body
    Text {
        /// Name of the function
        name: AsmName,
        /// Should we export this so other files can use this?
        exported: bool,
        /// Actual text of the function body
        instructions: Vec<AsmInstruction>,
    },
    /// Runtime-mutable data
    Data {
        /// Name of the data
        name: AsmName,
        /// Should we export this so other files can use this?
        exported: bool,
        /// Alignment requirement for this data
        alignment: NonZeroU64,
        /// Actual data
        data: Vec<AsmData>,
    },
    /// Runtime-immutable data
    RoData {
        /// Name of the data
        name: AsmName,
        /// Should we export this so other files can use this?
        exported: bool,
        /// Alignment requirement for this data
        alignment: NonZeroU64,
        /// Actual data
        data: Vec<AsmData>,
    },
    /// Runtime-mutable data, zero initialized
    Bss {
        /// Name of the data
        name: AsmName,
        /// Should we export this so other files can use this?
        exported: bool,
        /// Alignment requirement for this data
        alignment: NonZeroU64,
        /// Size of this data
        size: NonZeroU64,
    },
}
impl AsmFrag {
    fn section_header(&self) -> &str {
        match self {
            Self::Text { .. } => "section .text",
            Self::Data { .. } => "section .data",
            Self::RoData { .. } => "section .rodata",
            Self::Bss { .. } => "section .bss",
        }
    }
    fn size_of(data: &[AsmData]) -> u64 {
        data.iter().map(|datum| datum.size_of()).sum()
    }
}
impl Display for AsmFrag {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Text {
                name,
                exported,
                instructions,
            } => {
                if *exported {
                    writeln!(f, "global {name}:function ({name}.end - {name})")?;
                }
                writeln!(f, "{name}:")?;
                for instruction in instructions {
                    writeln!(f, "\t{instruction}")?;
                }
                if *exported {
                    writeln!(f, ".end:")?;
                }
                Ok(())
            }
            Self::Data {
                name,
                exported,
                alignment,
                data,
            }
            | Self::RoData {
                name,
                exported,
                alignment,
                data,
            } => {
                writeln!(f, "align {alignment}")?;
                if *exported {
                    writeln!(f, "global {name}:data ({})", AsmFrag::size_of(data))?;
                }
                writeln!(f, "{name}:")?;
                for datum in data {
                    writeln!(f, "\t{datum}")?;
                }
                if *exported {
                    writeln!(f, ".end:")?;
                }
                Ok(())
            }
            Self::Bss {
                name,
                exported,
                alignment,
                size,
            } => {
                if *exported {
                    writeln!(f, "global {name}:data ({size})")?;
                }
                write!(f, "align {alignment}\n{name}:\n\tresb {size}\n")
            }
        }
    }
}
impl Ord for AsmFrag {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Self::Text { .. }, Self::Text { .. })
            | (Self::Data { .. }, Self::Data { .. })
            | (Self::RoData { .. }, Self::RoData { .. })
            | (Self::Bss { .. }, Self::Bss { .. }) => Ordering::Equal,
            (Self::Text { .. }, Self::Data { .. })
            | (Self::Text { .. }, Self::RoData { .. })
            | (Self::Text { .. }, Self::Bss { .. })
            | (Self::Data { .. }, Self::RoData { .. })
            | (Self::Data { .. }, Self::Bss { .. })
            | (Self::RoData { .. }, Self::Bss { .. }) => Ordering::Less,
            (Self::Data { .. }, Self::Text { .. })
            | (Self::RoData { .. }, Self::Text { .. })
            | (Self::RoData { .. }, Self::Data { .. })
            | (Self::Bss { .. }, Self::Text { .. })
            | (Self::Bss { .. }, Self::Data { .. })
            | (Self::Bss { .. }, Self::RoData { .. }) => Ordering::Greater,
        }
    }
}
impl PartialOrd for AsmFrag {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Eq for AsmFrag {}
impl PartialEq for AsmFrag {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

/// A label
#[derive(Debug)]
pub enum AsmName {
    /// A globally exported label
    Global(String),
    /// A file-specific label
    Local(NonZeroU64),
}
impl Display for AsmName {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Global(global) => write!(f, "{global}"),
            Self::Local(local) => write!(f, "L{local}"),
        }
    }
}

/// Compile-time data
#[derive(Debug)]
pub enum AsmData {
    U8(u8),
    I8(i8),
    U16(u16),
    I16(i16),
    U32(u32),
    I32(i32),
    U64(u64),
    I64(i64),
    F32(f32),
    F64(f64),
    /// A UTF-32 codepoint
    Char(char),
    /// A UTF-8 character string
    String(String),
    /// A label, local or global
    Label(AsmName),
    /// Uninitialized space
    Padding(NonZeroU64),
}
impl AsmData {
    fn size_of(&self) -> u64 {
        match self {
            Self::U8(_) | Self::I8(_) => 1,
            Self::U16(_) | Self::I16(_) => 2,
            Self::U32(_) | Self::I32(_) | Self::F32(_) | Self::Char(_) => 4,
            Self::U64(_) | Self::I64(_) | Self::F64(_) | Self::Label(_) => 8,
            Self::String(s) => s.as_bytes().len() as u64,
            Self::Padding(n) => n.get(),
        }
    }
}
/// In alternate mode, formats for an operand instead of a data frag
impl Display for AsmData {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            match self {
                AsmData::U8(v) => write!(f, "{v}"),
                AsmData::I8(v) => write!(f, "{v}"),
                AsmData::U16(v) => write!(f, "{v}"),
                AsmData::I16(v) => write!(f, "{v}"),
                AsmData::U32(v) => write!(f, "{v}"),
                AsmData::I32(v) => write!(f, "{v}"),
                AsmData::U64(v) => write!(f, "{v}"),
                AsmData::I64(v) => write!(f, "{v}"),
                AsmData::F32(v) => write!(f, "__?float32?__({v})"),
                AsmData::F64(v) => write!(f, "__?float64?__({v})"),
                AsmData::Char(v) => write!(f, "{}", *v as u32),
                AsmData::String(_) => {
                    internal_error!("attempted to use string constant as operand")
                }
                AsmData::Label(v) => write!(f, "{v}"),
                AsmData::Padding(_) => {
                    internal_error!("attempted to use padding as operand")
                }
            }
        } else {
            match self {
                Self::U8(v) => write!(f, "db {v}"),
                Self::I8(v) => write!(f, "db {v}"),
                Self::U16(v) => write!(f, "dw {v}"),
                Self::I16(v) => write!(f, "dw {v}"),
                Self::U32(v) => write!(f, "dd {v}"),
                Self::I32(v) => write!(f, "dd {v}"),
                Self::U64(v) => write!(f, "dq {v}"),
                Self::I64(v) => write!(f, "dq {v}"),
                Self::F32(v) => write!(f, "dd {v}"),
                Self::F64(v) => write!(f, "dq {v}"),
                Self::Char(v) => {
                    if v.is_alphanumeric() {
                        write!(f, "dd '{v}'")
                    } else {
                        write!(f, "dd {}", *v as u32)
                    }
                }
                Self::String(v) => {
                    if v.chars().all(char::is_alphanumeric) {
                        write!(f, "db '{v}'")
                    } else {
                        write!(f, "db `")?;
                        for c in v.chars() {
                            if c.is_alphanumeric() {
                                write!(f, "{c}")?;
                            } else if (c as u32) < 0x10000 {
                                write!(f, "\\u{:04x}", c as u32)?;
                            } else {
                                write!(f, "\\U{:08x}", c as u32)?;
                            }
                        }
                        write!(f, "`")
                    }
                }
                Self::Label(v) => write!(f, "dq {v}"),
                Self::Padding(v) => write!(f, "resb {v}"),
            }
        }
    }
}

/// An instruction
#[derive(Debug)]
pub enum AsmInstruction {
    /// Regular instruction - not the target of a jump and control flow
    /// unconditionally proceeds to the next instruction
    RegularInstruction {
        skeleton: &'static str,
        operands: Vec<AsmOperand>,
    },
    /// Move instruction - a regular instruction, but can be elided if the
    /// first read-from operand is allocated the same actual location as the
    /// first written-to operand
    MoveInstruction {
        skeleton: &'static str,
        operands: Vec<AsmOperand>,
    },
    /// Jump instruction - control flow proceeds to the local label named in
    /// the target
    JumpInstruction {
        skeleton: &'static str,
        operands: Vec<AsmOperand>,
        target: NonZeroU64,
    },
    /// Conditional jump - control flow might proceed normally or might go to
    /// the named target
    ConditionalJumpInstruction {
        skeleton: &'static str,
        operands: Vec<AsmOperand>,
        target: NonZeroU64,
    },
    /// A jump table - control flow goes to one of the named targets
    JumpTableInstruction {
        skeleton: &'static str,
        operands: Vec<AsmOperand>,
        targets: Vec<NonZeroU64>,
    },
    /// A terminal instruction - control flow leaves this function
    LeaveInstruction {
        skeleton: &'static str,
        operands: Vec<AsmOperand>,
    },
    /// A local label - can be the target of a jump
    LabelInstruction {
        skeleton: &'static str,
        /// Must be exactly one Constant(Label(_))
        operands: Vec<AsmOperand>,
        name: NonZeroU64,
    },
}
impl Display for AsmInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        fn write_operand(
            f: &mut Formatter<'_>,
            operands: &[AsmOperand],
            arg_number: usize,
            skeleton: &str,
        ) -> fmt::Result {
            write!(
                f,
                "{}",
                operands.get(arg_number).unwrap_or_else(|| internal_error!(
                    "operand index {arg_number:?} out of range for skeleton {skeleton:?}"
                ))
            )
        }

        match self {
            Self::RegularInstruction { skeleton, operands }
            | Self::MoveInstruction { skeleton, operands }
            | Self::JumpInstruction {
                skeleton, operands, ..
            }
            | Self::ConditionalJumpInstruction {
                skeleton, operands, ..
            }
            | Self::JumpTableInstruction {
                skeleton, operands, ..
            }
            | Self::LeaveInstruction { skeleton, operands }
            | Self::LabelInstruction {
                skeleton, operands, ..
            } => {
                let mut escaped = false;
                let mut arg_number = 0_usize;
                for c in skeleton.chars() {
                    if !escaped {
                        if c == '`' {
                            escaped = true;
                            arg_number = 0;
                        } else {
                            write!(f, "{c}")?;
                        }
                    } else if let Some(digit) = c.to_digit(10) {
                        arg_number *= 10;
                        arg_number += digit as usize;
                    } else {
                        write_operand(f, operands, arg_number, skeleton)?;
                        write!(f, "{c}")?;
                        escaped = false;
                    }
                }
                if escaped {
                    if skeleton.ends_with('`') {
                        internal_error!("unterminated operand escape for skeleton {skeleton:?}");
                    }
                    write_operand(f, operands, arg_number, skeleton)?;
                }
                Ok(())
            }
        }
    }
}

/// An operand to an assembly instruction
#[derive(Debug)]
pub enum AsmOperand {
    /// A specific, named register
    Register {
        name: AsmRegister,
        size: NonZeroU64,
        read: bool,
        write: bool,
    },
    /// A reference to the stack - equivalent to "[rsp + {offset}]"
    StackLocation {
        offset: u64,
        size: NonZeroU64,
        read: bool,
        write: bool,
    },
    /// A temporary location - is converted into a register or a stack
    /// reference based on the allocation type
    Temporary {
        name: NonZeroU64,
        alignment: NonZeroU64,
        size: NonZeroU64,
        allocation_type: AllocationType,
        read: bool,
        write: bool,
        /// Can we skip patching if this turned into a stack reference (only
        /// meaningful if allocation_type != Memory)
        no_patch: bool,
    },
    /// A reference to part of a temporary location on the stack - is converted
    /// into a stack reference after allocation; reads and writes count as
    /// reads and writes of the whole temporary
    TemporaryPart {
        /// Must be an actual temporary
        temporary: Box<AsmOperand>,
        /// Either a register-like thing, a constant, or both
        offset: Vec<AsmOperand>,
        size: NonZeroU64,
        read: bool,
        write: bool,
    },
    /// Constant data - must be a valid immediate operand for this particular
    /// usage
    Constant(AsmData),
}
impl Display for AsmOperand {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Register { name, size, .. } => write!(
                f,
                "{}",
                match name {
                    AsmRegister::RAX => match size.get() {
                        1 => "al",
                        2 => "ax",
                        4 => "eax",
                        8 => "rax",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::RBX => match size.get() {
                        1 => "bl",
                        2 => "bx",
                        4 => "ebx",
                        8 => "rbx",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::RCX => match size.get() {
                        1 => "cl",
                        2 => "cx",
                        4 => "ecx",
                        8 => "rcx",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::RDX => match size.get() {
                        1 => "dl",
                        2 => "dx",
                        4 => "edx",
                        8 => "rdx",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::RSI => match size.get() {
                        1 => "sil",
                        2 => "si",
                        4 => "esi",
                        8 => "rsi",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::RDI => match size.get() {
                        1 => "dil",
                        2 => "di",
                        4 => "edi",
                        8 => "rdi",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::RSP => match size.get() {
                        1 => "spl",
                        2 => "sp",
                        4 => "esp",
                        8 => "rsp",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::RBP => match size.get() {
                        1 => "bpl",
                        2 => "bp",
                        4 => "ebp",
                        8 => "rbp",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::R8 => match size.get() {
                        1 => "r8b",
                        2 => "r8w",
                        4 => "r8d",
                        8 => "r8",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::R9 => match size.get() {
                        1 => "r9b",
                        2 => "r9w",
                        4 => "r9d",
                        8 => "r9",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::R10 => match size.get() {
                        1 => "r10b",
                        2 => "r10w",
                        4 => "r10d",
                        8 => "r10",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::R11 => match size.get() {
                        1 => "r11b",
                        2 => "r11w",
                        4 => "r11d",
                        8 => "r11",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::R12 => match size.get() {
                        1 => "r12b",
                        2 => "r12w",
                        4 => "r12d",
                        8 => "r12",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::R13 => match size.get() {
                        1 => "r13b",
                        2 => "r13w",
                        4 => "r13d",
                        8 => "r13",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::R14 => match size.get() {
                        1 => "r14b",
                        2 => "r14w",
                        4 => "r14d",
                        8 => "r14",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::R15 => match size.get() {
                        1 => "r15b",
                        2 => "r15w",
                        4 => "r15d",
                        8 => "r15",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::XMM0 => match size.get() {
                        4 | 8 | 16 => "xmm0",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::XMM1 => match size.get() {
                        4 | 8 | 16 => "xmm1",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::XMM2 => match size.get() {
                        4 | 8 | 16 => "xmm2",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::XMM3 => match size.get() {
                        4 | 8 | 16 => "xmm3",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::XMM4 => match size.get() {
                        4 | 8 | 16 => "xmm4",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::XMM5 => match size.get() {
                        4 | 8 | 16 => "xmm5",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::XMM6 => match size.get() {
                        4 | 8 | 16 => "xmm6",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::XMM7 => match size.get() {
                        4 | 8 | 16 => "xmm7",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::XMM8 => match size.get() {
                        4 | 8 | 16 => "xmm8",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::XMM9 => match size.get() {
                        4 | 8 | 16 => "xmm9",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::XMM10 => match size.get() {
                        4 | 8 | 16 => "xmm10",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::XMM11 => match size.get() {
                        4 | 8 | 16 => "xmm11",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::XMM12 => match size.get() {
                        4 | 8 | 16 => "xmm12",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::XMM13 => match size.get() {
                        4 | 8 | 16 => "xmm13",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::XMM14 => match size.get() {
                        4 | 8 | 16 => "xmm14",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::XMM15 => match size.get() {
                        4 | 8 | 16 => "xmm15",
                        size => internal_error!("invalid register size {size:?}"),
                    },
                    AsmRegister::RFLAGS =>
                        internal_error!("tried to display rflags as explicit operand"),
                }
            ),
            Self::StackLocation { offset, .. } => write!(f, "[rsp + {offset}]"),
            Self::Constant(data) => write!(f, "{data:#}"),
            Self::Temporary { .. } | Self::TemporaryPart { .. } => {
                internal_error!("tried to display temporary operand")
            }
        }
    }
}

/// What sort of actual location does this need
#[derive(Debug)]
pub enum AllocationType {
    General,
    Floating,
    Memory,
}

/// List of valid register names
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AsmRegister {
    RAX,
    RBX,
    RCX,
    RDX,
    RSI,
    RDI,
    RSP,
    RBP,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    XMM0,
    XMM1,
    XMM2,
    XMM3,
    XMM4,
    XMM5,
    XMM6,
    XMM7,
    XMM8,
    XMM9,
    XMM10,
    XMM11,
    XMM12,
    XMM13,
    XMM14,
    XMM15,
    RFLAGS,
}

/// Given some low-level IR, produce the corresponding x86_64 nasm assembly
pub fn backend(low_level_ir: &LowLevelIR, settings: &Settings) -> FileContents {
    let mut asm = to_temp_asm(low_level_ir);

    // TODO: temp-level ASM optimization

    allocate_temporary_locations(&mut asm);

    // TODO: register-level ASM optimization

    FileContents {
        filename: format!("{}{}.s", settings.setting_output_prefix, asm.filename),
        contents: format!("{asm}"),
    }
}

#[cfg(test)]
mod tests {
    use std::f64;

    use super::*;

    #[test]
    fn test_simple_asm_output() {
        let asm = Asm {
            filename: "file.h0",
            externs: vec![String::from("tau")],
            blocks: vec![
                AsmFrag::Data {
                    name: AsmName::Global(String::from("pi")),
                    exported: true,
                    alignment: 8.try_into().unwrap(),
                    data: vec![AsmData::F64(f64::consts::PI)],
                },
                AsmFrag::Text {
                    name: AsmName::Global(String::from("frob")),
                    exported: true,
                    instructions: vec![
                        AsmInstruction::RegularInstruction {
                            skeleton: "frob `0, `1",
                            operands: vec![
                                AsmOperand::Register {
                                    name: AsmRegister::RAX,
                                    size: 8.try_into().unwrap(),
                                    read: false,
                                    write: true,
                                },
                                AsmOperand::Register {
                                    name: AsmRegister::RDX,
                                    size: 8.try_into().unwrap(),
                                    read: false,
                                    write: true,
                                },
                            ],
                        },
                        AsmInstruction::RegularInstruction {
                            skeleton: "nop",
                            operands: vec![],
                        },
                        AsmInstruction::LeaveInstruction {
                            skeleton: "ret",
                            operands: vec![
                                AsmOperand::Register {
                                    name: AsmRegister::RAX,
                                    size: 8.try_into().unwrap(),
                                    read: true,
                                    write: false,
                                },
                                AsmOperand::Register {
                                    name: AsmRegister::RDX,
                                    size: 8.try_into().unwrap(),
                                    read: true,
                                    write: false,
                                },
                            ],
                        },
                    ],
                },
            ],
        };
        assert_eq!(
            format!("{asm}"),
            "
extern tau
section .text
global frob:function (frob.end - frob)
frob:
\tfrob rax, rdx
\tnop
\tret
.end:
section .data
align 8
global pi:data (8)
pi:
\tdq 3.141592653589793
.end:
section .comment
db \"Half-Zero version 0.3.1\"
section .note.GNU-stack noalloc noexec nowrite progbits
"
            .trim_start()
        );
    }
}
