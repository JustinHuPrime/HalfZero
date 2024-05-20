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
    collections::{HashMap, HashSet, VecDeque},
    marker::PhantomData,
    num::NonZeroU64,
    ptr,
};

use crate::diagnostics::internal_error;

use super::{Asm, AsmFrag, AsmInstruction, AsmOperand, AsmRegister};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Location {
    Register(AsmRegister),
    Temporary(NonZeroU64),
    StackLocation(u64),
}

/// The live-after information for some assembly
struct LivenessAnnotation<'asm> {
    mapping: HashMap<*const AsmInstruction, HashSet<Location>>,
    phantom: PhantomData<&'asm [AsmInstruction]>,
}
impl<'asm> LivenessAnnotation<'asm> {
    fn new(instructions: &'asm [AsmInstruction]) -> Self {
        let mut live_before = instructions
            .iter()
            .map(|instruction| {
                (
                    instruction as *const AsmInstruction,
                    HashSet::<Location>::new(),
                )
            })
            .collect::<HashMap<_, _>>();
        let mut live_after = instructions
            .iter()
            .map(|instruction| {
                (
                    instruction as *const AsmInstruction,
                    HashSet::<Location>::new(),
                )
            })
            .collect::<HashMap<_, _>>();

        // Example 2: the live-before set of an instruction removes anything
        //            that is written to in the instruction
        // mov rax, 0 // live-after = [rax, t0] => live-before = [t0]

        // Example 3: the live-before set of an instruction adds anything that
        //            is read from in the instruction (and this overrides
        //            removal from writing too)
        // test rax, rax // live-after = [t0] => live-before = [rax, t0]
        // inc rax // live-after = [t0] => live-before = [rax, t0]
        //         // but also, this is dead code, and should be removed in
        //         // the temp-level ASM optimization pass

        // Example 4: the live-before set of an instruction in a cycle is the
        //            least-fixed-point
        // label: // live-after = [rax, t0] => live-before = [rax, t0]
        // frob t0 // live-after = [rax, t0] => live-before = [rax, t0]
        // frob rax // live-after = [rax, t0] => live-before = [rax, t0]
        // cjump label // live-after = [rax, t0] => live-before = [rax, t0]

        loop {
            // breadth-first traverse through the instructions once and flow
            // the liveness information with it, starting at leave instructions

            let mut changed = false;
            let mut visited = HashSet::<*const AsmInstruction>::new();
            let mut next = instructions
                .iter()
                .filter(|i| matches!(i, AsmInstruction::LeaveInstruction { .. }))
                .collect::<VecDeque<&AsmInstruction>>();
            if next.is_empty() {
                internal_error!("found no leave instructions in {instructions:?}");
            }

            while let Some(current) = next.pop_front() {
                // repeat visit check
                if visited.contains(&(current as *const AsmInstruction)) {
                    continue;
                } else {
                    visited.insert(current);
                }

                fn get_next_instruction<'asm>(
                    instructions: &'asm [AsmInstruction],
                    current: &AsmInstruction,
                ) -> &'asm AsmInstruction {
                    let index = instructions
                        .iter()
                        .position(|instruction| ptr::eq(instruction, current))
                        .unwrap_or_else(|| {
                            internal_error!("could not find {current:?} in {instructions:?}")
                        });
                    instructions.get(index + 1).unwrap_or_else(|| {
                        internal_error!("no next instruction after {current:?} in {instructions:?}")
                    })
                }
                fn get_previous_instruction<'asm>(
                    instructions: &'asm [AsmInstruction],
                    current: &AsmInstruction,
                ) -> Option<&'asm AsmInstruction> {
                    let index = instructions
                        .iter()
                        .position(|instruction| ptr::eq(instruction, current))
                        .unwrap_or_else(|| {
                            internal_error!("could not find {current:?} in {instructions:?}")
                        });
                    if index == 0 {
                        None
                    } else {
                        instructions.get(index - 1)
                    }
                }
                fn get_jump_target<'asm>(
                    instructions: &'asm [AsmInstruction],
                    target: NonZeroU64,
                ) -> &'asm AsmInstruction {
                    instructions
                        .iter()
                        .find(|label| match label {
                            AsmInstruction::LabelInstruction { name, .. } => *name == target,
                            _ => false,
                        })
                        .unwrap_or_else(|| internal_error!("could not find jump target {target:?}"))
                }

                // do computation
                let operands = match current {
                    AsmInstruction::RegularInstruction { operands, .. }
                    | AsmInstruction::MoveInstruction { operands, .. }
                    | AsmInstruction::JumpInstruction { operands, .. }
                    | AsmInstruction::ConditionalJumpInstruction { operands, .. }
                    | AsmInstruction::JumpTableInstruction { operands, .. }
                    | AsmInstruction::LeaveInstruction { operands, .. }
                    | AsmInstruction::LabelInstruction { operands, .. } => operands,
                };
                let next_instructions = match current {
                    AsmInstruction::RegularInstruction { .. }
                    | AsmInstruction::MoveInstruction { .. }
                    | AsmInstruction::LabelInstruction { .. } => {
                        vec![get_next_instruction(instructions, current)]
                    }
                    AsmInstruction::JumpInstruction { target, .. } => {
                        vec![get_jump_target(instructions, *target)]
                    }
                    AsmInstruction::ConditionalJumpInstruction { target, .. } => {
                        vec![
                            get_next_instruction(instructions, current),
                            get_jump_target(instructions, *target),
                        ]
                    }
                    AsmInstruction::JumpTableInstruction { targets, .. } => targets
                        .iter()
                        .map(|target| get_jump_target(instructions, *target))
                        .collect(),
                    AsmInstruction::LeaveInstruction { .. } => Vec::new(),
                };

                // live-after = Union live_before[next_instructions[n]]
                let live_after_this = next_instructions
                    .iter()
                    .flat_map(|instruction| {
                        live_before
                            .get(&(*instruction as *const AsmInstruction))
                            .unwrap_or_else(|| {
                                internal_error!("could not find {instruction:?} in {live_before:?}")
                            })
                            .iter()
                            .cloned()
                    })
                    .collect::<HashSet<Location>>();

                // live-before = used(operands) union (live-after[this] - written(operands))
                let read_operands = operands
                    .iter()
                    .filter_map(|operand| match operand {
                        AsmOperand::Register { name, read, .. } if *read => {
                            Some(Location::Register(*name))
                        }
                        AsmOperand::StackLocation { offset, read, .. } if *read => {
                            Some(Location::StackLocation(*offset))
                        }
                        AsmOperand::Temporary { name, read, .. } if *read => {
                            Some(Location::Temporary(*name))
                        }
                        AsmOperand::TemporaryPart {
                            temporary, read, ..
                        } if *read => {
                            if let AsmOperand::Temporary { name, .. } = **temporary {
                                Some(Location::Temporary(name))
                            } else {
                                internal_error!("expected {temporary:?} to be a temporary value")
                            }
                        }
                        _ => None,
                    })
                    .collect::<HashSet<Location>>();
                let written_operands = operands
                    .iter()
                    .filter_map(|operand| match operand {
                        AsmOperand::Register { name, write, .. } if *write => {
                            Some(Location::Register(*name))
                        }
                        AsmOperand::StackLocation { offset, write, .. } if *write => {
                            Some(Location::StackLocation(*offset))
                        }
                        AsmOperand::Temporary { name, write, .. } if *write => {
                            Some(Location::Temporary(*name))
                        }
                        AsmOperand::TemporaryPart {
                            temporary, write, ..
                        } if *write => {
                            if let AsmOperand::Temporary { name, .. } = **temporary {
                                Some(Location::Temporary(name))
                            } else {
                                internal_error!("expected {temporary:?} to be a temporary value")
                            }
                        }
                        _ => None,
                    })
                    .collect::<HashSet<Location>>();
                let live_before_this = live_after_this
                    .difference(&written_operands)
                    .cloned()
                    .collect::<HashSet<Location>>()
                    .union(&read_operands)
                    .cloned()
                    .collect::<HashSet<Location>>();

                if live_after_this
                    != *live_after
                        .get(&(current as *const AsmInstruction))
                        .unwrap_or_else(|| {
                            internal_error!("could not find {current:?} in {live_after:?}")
                        })
                {
                    live_after.insert(current as *const AsmInstruction, live_after_this);
                    changed = true;
                }
                if live_before_this
                    != *live_before
                        .get(&(current as *const AsmInstruction))
                        .unwrap_or_else(|| {
                            internal_error!("could not find {current:?} in {live_before:?}")
                        })
                {
                    live_before.insert(current as *const AsmInstruction, live_before_this);
                    changed = true;
                }

                // add next nodes
                if let Some(previous @ AsmInstruction::RegularInstruction { .. })
                | Some(previous @ AsmInstruction::MoveInstruction { .. })
                | Some(previous @ AsmInstruction::ConditionalJumpInstruction { .. })
                | Some(previous @ AsmInstruction::LabelInstruction { .. }) =
                    get_previous_instruction(instructions, current)
                {
                    next.push_back(previous);
                }
                if let AsmInstruction::LabelInstruction { name, .. } = current {
                    for instruction in instructions {
                        match instruction {
                            AsmInstruction::JumpInstruction { target, .. }
                            | AsmInstruction::ConditionalJumpInstruction { target, .. }
                                if target == name =>
                            {
                                next.push_back(instruction);
                            }
                            AsmInstruction::JumpTableInstruction { targets, .. }
                                if targets.contains(name) =>
                            {
                                next.push_back(instruction);
                            }
                            _ => {}
                        }
                    }
                }
            }

            //  repeat previous step if there were changes
            if !changed {
                break;
            }
        }

        Self {
            mapping: live_after,
            phantom: PhantomData,
        }
    }
    fn get<'a>(&'a self, instruction: &AsmInstruction) -> Option<&'a HashSet<Location>> {
        self.mapping.get(&(instruction as *const AsmInstruction))
    }
    fn get_mut<'a>(
        &'a mut self,
        instruction: &AsmInstruction,
    ) -> Option<&'a mut HashSet<Location>> {
        self.mapping
            .get_mut(&(instruction as *const AsmInstruction))
    }
}

/// Converts all temporaries into concrete locations
pub fn allocate_temporary_locations(asm: &mut Asm) {
    for block in asm.blocks.iter_mut() {
        if let AsmFrag::Text { instructions, .. } = block {
            loop {
                // generate live-before and live-after annotations

                let liveness = LivenessAnnotation::new(instructions);

                // convert to interference graph

                todo!();

                // attempt assignment

                todo!();

                // if failed, spill, patch, and retry

                todo!();

                // assign remaining temps to concrete locations
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::backend::x86_64_linux::{AllocationType, AsmData, AsmName};

    use super::*;

    #[test]
    fn test_live_before_based_on_live_after() {
        let asm = vec![
            AsmInstruction::RegularInstruction {
                skeleton: "frob `0, `1",
                operands: vec![
                    AsmOperand::Register {
                        name: AsmRegister::RAX,
                        size: 8.try_into().unwrap(),
                        read: false,
                        write: true,
                    },
                    AsmOperand::Temporary {
                        name: 1.try_into().unwrap(),
                        alignment: 8.try_into().unwrap(),
                        size: 8.try_into().unwrap(),
                        allocation_type: AllocationType::General,
                        read: false,
                        write: true,
                        no_patch: false,
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
                    AsmOperand::Temporary {
                        name: 1.try_into().unwrap(),
                        alignment: 8.try_into().unwrap(),
                        size: 8.try_into().unwrap(),
                        allocation_type: AllocationType::General,
                        read: true,
                        write: false,
                        no_patch: false,
                    },
                ],
            },
        ];

        let liveness = LivenessAnnotation::new(&asm);

        assert_eq!(
            *liveness.get(&asm[1]).unwrap(),
            HashSet::from([
                Location::Register(AsmRegister::RAX),
                Location::Temporary(1.try_into().unwrap())
            ])
        );
    }

    #[test]
    fn test_subtract_written_to() {
        let asm = vec![
            AsmInstruction::RegularInstruction {
                skeleton: "frob `0, `1",
                operands: vec![
                    AsmOperand::Register {
                        name: AsmRegister::RAX,
                        size: 8.try_into().unwrap(),
                        read: false,
                        write: true,
                    },
                    AsmOperand::Temporary {
                        name: 1.try_into().unwrap(),
                        alignment: 8.try_into().unwrap(),
                        size: 8.try_into().unwrap(),
                        allocation_type: AllocationType::General,
                        read: false,
                        write: true,
                        no_patch: false,
                    },
                ],
            },
            AsmInstruction::RegularInstruction {
                skeleton: "frob `0",
                operands: vec![AsmOperand::Temporary {
                    name: 1.try_into().unwrap(),
                    alignment: 8.try_into().unwrap(),
                    size: 8.try_into().unwrap(),
                    allocation_type: AllocationType::General,
                    read: false,
                    write: true,
                    no_patch: false,
                }],
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
                    AsmOperand::Temporary {
                        name: 1.try_into().unwrap(),
                        alignment: 8.try_into().unwrap(),
                        size: 8.try_into().unwrap(),
                        allocation_type: AllocationType::General,
                        read: true,
                        write: false,
                        no_patch: false,
                    },
                ],
            },
        ];

        let liveness = LivenessAnnotation::new(&asm);

        assert_eq!(
            *liveness.get(&asm[0]).unwrap(),
            HashSet::from([Location::Register(AsmRegister::RAX)])
        );
    }

    #[test]
    fn test_add_read_from() {
        let asm = vec![
            AsmInstruction::RegularInstruction {
                skeleton: "frob `0, `1",
                operands: vec![
                    AsmOperand::Register {
                        name: AsmRegister::RAX,
                        size: 8.try_into().unwrap(),
                        read: false,
                        write: true,
                    },
                    AsmOperand::Temporary {
                        name: 1.try_into().unwrap(),
                        alignment: 8.try_into().unwrap(),
                        size: 8.try_into().unwrap(),
                        allocation_type: AllocationType::General,
                        read: false,
                        write: true,
                        no_patch: false,
                    },
                ],
            },
            AsmInstruction::RegularInstruction {
                skeleton: "frob `0, `1",
                operands: vec![
                    AsmOperand::Temporary {
                        name: 1.try_into().unwrap(),
                        alignment: 8.try_into().unwrap(),
                        size: 8.try_into().unwrap(),
                        allocation_type: AllocationType::General,
                        read: false,
                        write: true,
                        no_patch: false,
                    },
                    AsmOperand::Temporary {
                        name: 1.try_into().unwrap(),
                        alignment: 8.try_into().unwrap(),
                        size: 8.try_into().unwrap(),
                        allocation_type: AllocationType::General,
                        read: true,
                        write: false,
                        no_patch: false,
                    },
                ],
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
                    AsmOperand::Temporary {
                        name: 1.try_into().unwrap(),
                        alignment: 8.try_into().unwrap(),
                        size: 8.try_into().unwrap(),
                        allocation_type: AllocationType::General,
                        read: true,
                        write: false,
                        no_patch: false,
                    },
                ],
            },
        ];

        let liveness = LivenessAnnotation::new(&asm);

        assert_eq!(
            *liveness.get(&asm[0]).unwrap(),
            HashSet::from([
                Location::Register(AsmRegister::RAX),
                Location::Temporary(1.try_into().unwrap())
            ])
        );
    }

    #[test]
    fn test_loop_least_fixed_point() {
        let asm = vec![
            AsmInstruction::LabelInstruction {
                skeleton: "`0:",
                operands: vec![AsmOperand::Constant(AsmData::Label(AsmName::Local(
                    2.try_into().unwrap(),
                )))],
                name: 2.try_into().unwrap(),
            },
            AsmInstruction::RegularInstruction {
                skeleton: "frob `0",
                operands: vec![AsmOperand::Temporary {
                    name: 1.try_into().unwrap(),
                    alignment: 8.try_into().unwrap(),
                    size: 8.try_into().unwrap(),
                    allocation_type: AllocationType::General,
                    read: true,
                    write: false,
                    no_patch: false,
                }],
            },
            AsmInstruction::RegularInstruction {
                skeleton: "frob `0",
                operands: vec![AsmOperand::Temporary {
                    name: 1.try_into().unwrap(),
                    alignment: 8.try_into().unwrap(),
                    size: 8.try_into().unwrap(),
                    allocation_type: AllocationType::General,
                    read: false,
                    write: true,
                    no_patch: false,
                }],
            },
            AsmInstruction::ConditionalJumpInstruction {
                skeleton: "cjump `0",
                operands: vec![AsmOperand::Constant(AsmData::Label(AsmName::Local(
                    2.try_into().unwrap(),
                )))],
                target: 2.try_into().unwrap(),
            },
            AsmInstruction::LeaveInstruction {
                skeleton: "ret",
                operands: vec![AsmOperand::Register {
                    name: AsmRegister::RAX,
                    size: 8.try_into().unwrap(),
                    read: true,
                    write: false,
                }],
            },
        ];

        let liveness = LivenessAnnotation::new(&asm);

        assert_eq!(
            *liveness.get(&asm[0]).unwrap(),
            HashSet::from([
                Location::Register(AsmRegister::RAX),
                Location::Temporary(1.try_into().unwrap())
            ])
        );
        assert_eq!(
            *liveness.get(&asm[1]).unwrap(),
            HashSet::from([Location::Register(AsmRegister::RAX),])
        );
        assert_eq!(
            *liveness.get(&asm[2]).unwrap(),
            HashSet::from([
                Location::Register(AsmRegister::RAX),
                Location::Temporary(1.try_into().unwrap())
            ])
        );
        assert_eq!(
            *liveness.get(&asm[3]).unwrap(),
            HashSet::from([
                Location::Register(AsmRegister::RAX),
                Location::Temporary(1.try_into().unwrap())
            ])
        );
        assert_eq!(*liveness.get(&asm[4]).unwrap(), HashSet::from([]));
    }
}
