#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Target {
    Register(Reg),
    IndirectHL,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Value {
    Register(Reg),
    Immediate(u8),
    IndirectHL,
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
pub(crate) enum Instruction {
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

    Jump {
        condition: JumpCondition,
        target: JumpTarget,
    },
    AddStackPointer(i8),
}

impl Instruction {
    pub(crate) fn from_opcode(opcode: u8) -> Self {
        match opcode {
            0x00 => Self::Nop,
            //
            0x03 => Self::Inc16 { reg: Reg16::BC },
            0x04 => Self::Inc {
                target: Target::Register(Reg::B),
            },
            0x05 => Self::Dec {
                target: Target::Register(Reg::B),
            },
            //
            0x07 => Self::RotateLeft {
                reg: Reg::A,
                through_carry: false,
            },
            //
            0x09 => Self::AddHL { reg: Reg16::BC },
            //
            0x0B => Self::Dec16 { reg: Reg16::BC },
            0x0C => Self::Inc {
                target: Target::Register(Reg::C),
            },
            0x0D => Self::Dec {
                target: Target::Register(Reg::C),
            },
            0x0A => Self::RotateRight {
                reg: Reg::A,
                through_carry: false,
            },
            ////
            0x10 => Self::Stop,
            //
            0x13 => Self::Inc16 { reg: Reg16::DE },
            0x14 => Self::Inc {
                target: Target::Register(Reg::D),
            },
            0x15 => Self::Dec {
                target: Target::Register(Reg::D),
            },
            //
            0x17 => Self::RotateLeft {
                reg: Reg::A,
                through_carry: true,
            },
            //
            0x19 => Self::AddHL { reg: Reg16::DE },
            //
            0x1B => Self::Dec16 { reg: Reg16::DE },
            0x1C => Self::Inc {
                target: Target::Register(Reg::E),
            },
            0x1D => Self::Dec {
                target: Target::Register(Reg::E),
            },
            0x1A => Self::RotateRight {
                reg: Reg::A,
                through_carry: true,
            },
            ////
            0x23 => Self::Inc16 { reg: Reg16::HL },
            0x24 => Self::Inc {
                target: Target::Register(Reg::H),
            },
            0x25 => Self::Dec {
                target: Target::Register(Reg::H),
            },
            //
            0x27 => Self::DecimalAdjustAccumulator,
            //
            0x29 => Self::AddHL { reg: Reg16::HL },
            //
            0x2B => Self::Dec16 { reg: Reg16::HL },
            0x2C => Self::Inc {
                target: Target::Register(Reg::L),
            },
            0x2D => Self::Dec {
                target: Target::Register(Reg::L),
            },
            0x2A => Self::ComplementAccumulator,
            ////
            0x33 => Self::Inc16 { reg: Reg16::SP },
            0x34 => Self::Inc {
                target: Target::IndirectHL,
            },
            0x35 => Self::Dec {
                target: Target::IndirectHL,
            },
            //
            0x37 => Self::SetCarryFlag,
            //
            0x39 => Self::AddHL { reg: Reg16::SP },
            //
            0x3B => Self::Dec16 { reg: Reg16::SP },
            0x3C => Self::Inc {
                target: Target::Register(Reg::A),
            },
            0x3D => Self::Dec {
                target: Target::Register(Reg::A),
            },
            0x3A => Self::ComplementCarryFlag,
            _ => {
                panic!("Unkown instruction found for: 0x{:x}", opcode);
            }
        }
    }

    pub(crate) fn from_prefixed(opcode: u8) -> Instruction {
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
