use crate::cpu::CPU;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Target {
    Register(Reg),
    Address(u16),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Value {
    Register(Reg),
    Immediate(u8),
    Address(u16),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum JumpCondition {
    NotZero,
    Zero,
    Carry,
    NotCarry,
    Always,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum JumpTarget {
    HL,
    Absolute(u16),
    Relative(i8),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LoadHTarget {
    Register(Reg),
    RegisterIndirect(Reg),
    Immediate(u8),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Instr {
    Nop,
    Stop,
    Add {
        value: Value,
        carry: bool,
    },
    Sub {
        value: Value,
        carry: bool,
    },
    Compare {
        value: Value,
    },
    Inc {
        target: Target,
    },
    Dec {
        target: Target,
    },
    And {
        value: Value,
    },
    Or {
        value: Value,
    },
    Xor {
        value: Value,
    },
    ComplementCarryFlag,
    SetCarryFlag,
    ComplementAccumulator,
    DecimalAdjustAccumulator,
    Inc16 {
        reg: Reg16,
    },
    Dec16 {
        reg: Reg16,
    },
    AddHL {
        reg: Reg16,
    },

    Swap {
        reg: Reg,
    },
    RotateRight {
        reg: Reg,
        through_carry: bool,
    },
    RotateLeft {
        reg: Reg,
        through_carry: bool,
    },
    ShiftRight {
        reg: Reg,
        set_msb_zero: bool,
    },
    ShiftLeft {
        reg: Reg,
    },
    Bit {
        reg: Reg,
        bit: u8,
    },
    SetBit {
        reg: Reg,
        bit: u8,
    },
    ResetBit {
        reg: Reg,
        bit: u8,
    },
    Load {
        lhs: Target,
        rhs: Value,
    },
    LoadH {
        from: LoadHTarget,
        to: LoadHTarget,
    },

    Jump {
        condition: JumpCondition,
        target: JumpTarget,
    },
    AddStackPointer(i8),
}

impl CPU {
    pub(crate) fn decode(&mut self, opcode: u8) -> Instr {
        match opcode {
            0x00 => Instr::Nop,
            //
            0x03 => Instr::Inc16 { reg: Reg16::BC },
            0x04 => Instr::Inc {
                target: Target::Register(Reg::B),
            },
            0x05 => Instr::Dec {
                target: Target::Register(Reg::B),
            },
            //
            0x07 => Instr::RotateLeft {
                reg: Reg::A,
                through_carry: false,
            },
            //
            0x09 => Instr::AddHL { reg: Reg16::BC },
            //
            0x0B => Instr::Dec16 { reg: Reg16::BC },
            0x0C => Instr::Inc {
                target: Target::Register(Reg::C),
            },
            0x0D => Instr::Dec {
                target: Target::Register(Reg::C),
            },
            0x0A => Instr::RotateRight {
                reg: Reg::A,
                through_carry: false,
            },
            ////
            0x10 => Instr::Stop,
            //
            0x13 => Instr::Inc16 { reg: Reg16::DE },
            0x14 => Instr::Inc {
                target: Target::Register(Reg::D),
            },
            0x15 => Instr::Dec {
                target: Target::Register(Reg::D),
            },
            //
            0x17 => Instr::RotateLeft {
                reg: Reg::A,
                through_carry: true,
            },
            //
            0x19 => Instr::AddHL { reg: Reg16::DE },
            //
            0x1B => Instr::Dec16 { reg: Reg16::DE },
            0x1C => Instr::Inc {
                target: Target::Register(Reg::E),
            },
            0x1D => Instr::Dec {
                target: Target::Register(Reg::E),
            },
            0x1A => Instr::RotateRight {
                reg: Reg::A,
                through_carry: true,
            },
            ////
            0x23 => Instr::Inc16 { reg: Reg16::HL },
            0x24 => Instr::Inc {
                target: Target::Register(Reg::H),
            },
            0x25 => Instr::Dec {
                target: Target::Register(Reg::H),
            },
            //
            0x27 => Instr::DecimalAdjustAccumulator,
            //
            0x29 => Instr::AddHL { reg: Reg16::HL },
            //
            0x2B => Instr::Dec16 { reg: Reg16::HL },
            0x2C => Instr::Inc {
                target: Target::Register(Reg::L),
            },
            0x2D => Instr::Dec {
                target: Target::Register(Reg::L),
            },
            0x2A => Instr::ComplementAccumulator,
            ////
            0x33 => Instr::Inc16 { reg: Reg16::SP },
            0x34 => Instr::Inc {
                target: Target::Address(self.regs.hl()),
            },
            0x35 => Instr::Dec {
                target: Target::Address(self.regs.hl()),
            },
            //
            0x37 => Instr::SetCarryFlag,
            //
            0x39 => Instr::AddHL { reg: Reg16::SP },
            //
            0x3B => Instr::Dec16 { reg: Reg16::SP },
            0x3C => Instr::Inc {
                target: Target::Register(Reg::A),
            },
            0x3D => Instr::Dec {
                target: Target::Register(Reg::A),
            },
            0x3A => Instr::ComplementCarryFlag,
            _ => {
                panic!("Unkown instruction found for: 0x{:x}", opcode);
            }
        }
    }

    pub(crate) fn decode_prefixed(&mut self, opcode: u8) -> Instr {
        match opcode {
            _ => {
                panic!("Unkown instruction found for: 0xCB{:x}", opcode);
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Reg {
    A,
    B,
    C,
    D,
    E,
    F,
    H,
    L,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Reg16 {
    AF,
    BC,
    DE,
    HL,
    SP,
}
