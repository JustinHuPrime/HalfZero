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

use self::{register_allocator::allocate_registers, translator::to_temp_asm};

use super::FileContents;

pub mod register_allocator;
pub mod translator;

#[derive(Debug)]
pub struct Asm<'args> {
    filename: &'args str,
    externs: Vec<String>,
    blocks: Vec<AsmBlock>,
}
impl<'args> Display for Asm<'args> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut blocks = self.blocks.iter().collect::<Vec<_>>();
        blocks.sort();

        writeln!(f, "extern {}", self.externs.join(","))?;
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
        writeln!(f, "section .note.GNU-stack noalloc noexec nowrite progbits")?;
        Ok(())
    }
}

#[derive(Debug)]
pub enum AsmBlock {
    Text {
        name: AsmName,
        exported: bool,
        instructions: Vec<AsmInstruction>,
    },
    Data {
        name: AsmName,
        exported: bool,
        alignment: NonZeroU64,
        data: Vec<AsmData>,
    },
    RoData {
        name: AsmName,
        exported: bool,
        alignment: NonZeroU64,
        data: Vec<AsmData>,
    },
    Bss {
        name: AsmName,
        exported: bool,
        alignment: NonZeroU64,
        size: NonZeroU64,
    },
}
impl AsmBlock {
    fn section_header(&self) -> &str {
        match self {
            Self::Text { .. } => "section .text",
            Self::Data { .. } => "section .data",
            Self::RoData { .. } => "section .rodata",
            Self::Bss { .. } => "section .bss",
        }
    }
}
impl AsmBlock {
    fn size_of(data: &[AsmData]) -> u64 {
        data.iter().map(|datum| datum.size_of()).sum()
    }
}
impl Display for AsmBlock {
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
                    writeln!(f, "global {name}:data ({})", AsmBlock::size_of(data))?;
                }
                writeln!(f, "{name}")?;
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
                write!(f, "align {alignment}\n{name}\n\tresb {size}\n")
            }
        }
    }
}
impl Ord for AsmBlock {
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
impl PartialOrd for AsmBlock {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Eq for AsmBlock {}
impl PartialEq for AsmBlock {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

#[derive(Debug)]
pub enum AsmName {
    Global(String),
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
    Char(char),
    String(String),
    GlobalLabel(String),
    LocalLabel(NonZeroU64),
    Padding(NonZeroU64),
}
impl AsmData {
    fn size_of(&self) -> u64 {
        match self {
            Self::U8(_) | Self::I8(_) => 1,
            Self::U16(_) | Self::I16(_) => 2,
            Self::U32(_) | Self::I32(_) | Self::F32(_) | Self::Char(_) => 4,
            Self::U64(_)
            | Self::I64(_)
            | Self::F64(_)
            | Self::GlobalLabel(_)
            | Self::LocalLabel(_) => 8,
            Self::String(s) => s.as_bytes().len() as u64,
            Self::Padding(n) => n.get(),
        }
    }
}
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
                AsmData::String(_) => internal_error!(
                    file!(),
                    line!(),
                    "attempted to use string constant as operand"
                ),
                AsmData::GlobalLabel(v) => write!(f, "{v}"),
                AsmData::LocalLabel(v) => write!(f, "L{v}"),
                AsmData::Padding(_) => {
                    internal_error!(file!(), line!(), "attempted to use padding as operand")
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
                Self::GlobalLabel(v) => write!(f, "dq {v}"),
                Self::LocalLabel(v) => write!(f, "dq L{v}"),
                Self::Padding(v) => write!(f, "resb {v}"),
            }
        }
    }
}

#[derive(Debug)]
pub enum AsmInstruction {
    RegularInstruction {
        skeleton: &'static str,
        operands: Vec<AsmOperand>,
    },
    MoveInstruction {
        skeleton: &'static str,
        operands: Vec<AsmOperand>,
    },
    JumpInstruction {
        skeleton: &'static str,
        operands: Vec<AsmOperand>,
        target: NonZeroU64,
    },
    ConditionalJumpInstruction {
        skeleton: &'static str,
        operands: Vec<AsmOperand>,
        target: NonZeroU64,
    },
    JumpTableInstruction {
        skeleton: &'static str,
        operands: Vec<AsmOperand>,
        targets: Vec<NonZeroU64>,
    },
    LeaveInstruction {
        skeleton: &'static str,
        operands: Vec<AsmOperand>,
    },
    LabelInstruction {
        skeleton: &'static str,
        operands: Vec<AsmOperand>,
        name: Option<NonZeroU64>,
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
                    file!(),
                    line!(),
                    "operand index {arg_number} out of range for skeleton {skeleton}"
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
                        internal_error!(
                            file!(),
                            line!(),
                            "unterminated operand escape for skeleton {skeleton}"
                        );
                    }
                    write_operand(f, operands, arg_number, skeleton)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug)]
pub enum AsmOperand {
    Register {
        name: AsmRegister,
        size: NonZeroU64,
    },
    Temporary {
        name: NonZeroU64,
        alignment: NonZeroU64,
        size: NonZeroU64,
    },
    TemporaryPart {
        temporary: Box<AsmOperand>,
        offset: Box<AsmOperand>,
        alignment: NonZeroU64,
        size: NonZeroU64,
    },
    Constant(AsmData),
}
impl Display for AsmOperand {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Register { name, size } => write!(
                f,
                "{}",
                match name {
                    AsmRegister::RAX => match size.get() {
                        1 => "al",
                        2 => "ax",
                        4 => "eax",
                        8 => "rax",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::RBX => match size.get() {
                        1 => "bl",
                        2 => "bx",
                        4 => "ebx",
                        8 => "rbx",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::RCX => match size.get() {
                        1 => "cl",
                        2 => "cx",
                        4 => "ecx",
                        8 => "rcx",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::RDX => match size.get() {
                        1 => "dl",
                        2 => "dx",
                        4 => "edx",
                        8 => "rdx",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::RSI => match size.get() {
                        1 => "sil",
                        2 => "si",
                        4 => "esi",
                        8 => "rsi",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::RDI => match size.get() {
                        1 => "dil",
                        2 => "di",
                        4 => "edi",
                        8 => "rdi",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::RSP => match size.get() {
                        1 => "spl",
                        2 => "sp",
                        4 => "esp",
                        8 => "rsp",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::RBP => match size.get() {
                        1 => "bpl",
                        2 => "bp",
                        4 => "ebp",
                        8 => "rbp",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::R8 => match size.get() {
                        1 => "r8b",
                        2 => "r8w",
                        4 => "r8d",
                        8 => "r8",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::R9 => match size.get() {
                        1 => "r9b",
                        2 => "r9w",
                        4 => "r9d",
                        8 => "r9",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::R10 => match size.get() {
                        1 => "r10b",
                        2 => "r10w",
                        4 => "r10d",
                        8 => "r10",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::R11 => match size.get() {
                        1 => "r11b",
                        2 => "r11w",
                        4 => "r11d",
                        8 => "r11",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::R12 => match size.get() {
                        1 => "r12b",
                        2 => "r12w",
                        4 => "r12d",
                        8 => "r12",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::R13 => match size.get() {
                        1 => "r13b",
                        2 => "r13w",
                        4 => "r13d",
                        8 => "r13",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::R14 => match size.get() {
                        1 => "r14b",
                        2 => "r14w",
                        4 => "r14d",
                        8 => "r14",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::R15 => match size.get() {
                        1 => "r15b",
                        2 => "r15w",
                        4 => "r15d",
                        8 => "r15",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::XMM0 => match size.get() {
                        4 | 8 | 16 => "xmm0",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::XMM1 => match size.get() {
                        4 | 8 | 16 => "xmm1",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::XMM2 => match size.get() {
                        4 | 8 | 16 => "xmm2",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::XMM3 => match size.get() {
                        4 | 8 | 16 => "xmm3",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::XMM4 => match size.get() {
                        4 | 8 | 16 => "xmm4",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::XMM5 => match size.get() {
                        4 | 8 | 16 => "xmm5",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::XMM6 => match size.get() {
                        4 | 8 | 16 => "xmm6",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::XMM7 => match size.get() {
                        4 | 8 | 16 => "xmm7",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::XMM8 => match size.get() {
                        4 | 8 | 16 => "xmm8",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::XMM9 => match size.get() {
                        4 | 8 | 16 => "xmm9",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::XMM10 => match size.get() {
                        4 | 8 | 16 => "xmm10",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::XMM11 => match size.get() {
                        4 | 8 | 16 => "xmm11",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::XMM12 => match size.get() {
                        4 | 8 | 16 => "xmm12",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::XMM13 => match size.get() {
                        4 | 8 | 16 => "xmm13",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::XMM14 => match size.get() {
                        4 | 8 | 16 => "xmm14",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::XMM15 => match size.get() {
                        4 | 8 | 16 => "xmm15",
                        size => internal_error!(file!(), line!(), "invalid register size {size}"),
                    },
                    AsmRegister::RFLAGS => internal_error!(
                        file!(),
                        line!(),
                        "tried to display rflags as explicit operand"
                    ),
                }
            ),
            Self::Constant(data) => write!(f, "{data:#}"),
            Self::Temporary { .. } | Self::TemporaryPart { .. } => {
                internal_error!(file!(), line!(), "tried to display temporary operand")
            }
        }
    }
}

#[derive(Debug)]
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

pub fn backend(low_level_ir: &LowLevelIR, settings: &Settings) -> FileContents {
    let mut asm = to_temp_asm(low_level_ir);

    // TODO: temp-level ASM optimization

    allocate_registers(&mut asm);

    // TODO: register-level ASM optimization

    FileContents::new(
        format!("{}{}.s", settings.setting_output_prefix, asm.filename),
        format!("{asm}"),
    )
}