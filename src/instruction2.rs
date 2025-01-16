#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum R8 {
    B,
    C,
    D,
    E,
    H,
    L,
    HLInd,
    A,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum R16 {
    BC,
    DE,
    HL,
    SP,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum R16Stack {
    BC,
    DE,
    HL,
    AF,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum R16Mem {
    BC,
    DE,
    HLInc,
    HLDec,
    N16,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Condition {
    NotZero,
    Zero,
    NotCarry,
    Carry,
    Always,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ByteSource {
    R8(R8),
    N8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum JumpSource {
    HL,
    N16,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]

pub(crate) enum RSTAddress {
    X00,
    X08,
    X10,
    X18,
    X20,
    X28,
    X30,
    X38,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LoadHalfTarget {
    C,
    N16,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]

pub(crate) enum BitIndex {
    B0,
    B1,
    B2,
    B3,
    B4,
    B5,
    B6,
    B7,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Instr {
    Nop,
    Load(R8, ByteSource),
    Load16Immediate(R16),
    LoadIndirectToAcc(R16Mem),
    LoadAccToIndirect(R16Mem),
    LoadSPToIndirectImmediate,
    LoadHalfIndirectToAcc(LoadHalfTarget),
    LoadHalfAccToIndirect(LoadHalfTarget),
    Add(ByteSource),
    AddWithCarry(ByteSource),
    Sub(ByteSource),
    SubWithCarry(ByteSource),
    And(ByteSource),
    Xor(ByteSource),
    Or(ByteSource),
    Cmp(ByteSource),
    Inc(R8),
    Dec(R8),
    Add16(R16, R16),
    Inc16(R16),
    Dec16(R16),
    RotateLeft(R8),
    RotateRight(R8),
    RotateLeftThruCarry(R8),
    RotateRightThruCarry(R8),
    ShiftLeftArithmetic(R8),
    ShiftRightArithmetic(R8),
    Swap(R8),
    ShiftRightLogical(R8),
    Bit(BitIndex, R8),
    ResetBit(BitIndex, R8),
    SetBit(BitIndex, R8),
    DecimalAdjustAcc,
    ComplementAcc,
    SetCarryFlag,
    ClearCarryFlag,
    JumpRelative(Condition),
    JumpAbsolute(Condition, JumpSource),
    Stop,
    Halt,
    Ret(Condition),
    RetInterrupt,
    Call(Condition),
    CallRST(RSTAddress),
    Pop(R16Stack),
    Push(R16Stack),
    AddSP,
    LoadSPPlusImmediateToHL,
    LoadHLToSP,
    DisableInterrupt,
    EnableInterrupt,
}

impl Instr {
    pub(crate) fn from_opcode(opcode: u8) -> Self {
        match opcode {
            // BLOCK 0
            //  nop
            0x00 => Self::Nop,
            // ld r16, imm16
            0x01 => Self::Load16Immediate(R16::BC),
            0x11 => Self::Load16Immediate(R16::DE),
            0x21 => Self::Load16Immediate(R16::HL),
            0x31 => Self::Load16Immediate(R16::SP),
            // ld [r16mem], a
            0x02 => Self::LoadAccToIndirect(R16Mem::BC),
            0x12 => Self::LoadAccToIndirect(R16Mem::DE),
            0x22 => Self::LoadAccToIndirect(R16Mem::HLInc),
            0x32 => Self::LoadAccToIndirect(R16Mem::HLDec),
            // ld a, [r16mem]
            0x0A => Self::LoadIndirectToAcc(R16Mem::BC),
            0x1A => Self::LoadIndirectToAcc(R16Mem::DE),
            0x2A => Self::LoadIndirectToAcc(R16Mem::HLInc),
            0x3A => Self::LoadIndirectToAcc(R16Mem::HLDec),
            // ld [imm16], sp
            0x08 => Self::LoadSPToIndirectImmediate,
            // inc r16
            0x03 => Self::Inc16(R16::BC),
            0x13 => Self::Inc16(R16::DE),
            0x23 => Self::Inc16(R16::HL),
            0x33 => Self::Inc16(R16::SP),
            // dec r16
            0x0B => Self::Dec16(R16::BC),
            0x1B => Self::Dec16(R16::DE),
            0x2B => Self::Dec16(R16::HL),
            0x3B => Self::Dec16(R16::SP),
            // add hl, r16
            0x09 => Self::Add16(R16::HL, R16::BC),
            0x19 => Self::Add16(R16::HL, R16::DE),
            0x29 => Self::Add16(R16::HL, R16::HL),
            0x39 => Self::Add16(R16::HL, R16::SP),
            // inc r8
            0b0000_0100 => Self::Inc(R8::B),
            0b0000_1100 => Self::Inc(R8::C),
            0b0001_0100 => Self::Inc(R8::D),
            0b0001_1100 => Self::Inc(R8::E),
            0b0010_0100 => Self::Inc(R8::H),
            0b0010_1100 => Self::Inc(R8::L),
            0b0011_0100 => Self::Inc(R8::HLInd),
            0b0011_1100 => Self::Inc(R8::A),
            // dec r8
            0b0000_0101 => Self::Dec(R8::B),
            0b0000_1101 => Self::Dec(R8::C),
            0b0001_0101 => Self::Dec(R8::D),
            0b0001_1101 => Self::Dec(R8::E),
            0b0010_0101 => Self::Dec(R8::H),
            0b0010_1101 => Self::Dec(R8::L),
            0b0011_0101 => Self::Dec(R8::HLInd),
            0b0011_1101 => Self::Dec(R8::A),
            // ld r8, imm8
            0b0000_0110 => Self::Load(R8::B, ByteSource::N8),
            0b0000_1110 => Self::Load(R8::C, ByteSource::N8),
            0b0001_0110 => Self::Load(R8::D, ByteSource::N8),
            0b0001_1110 => Self::Load(R8::E, ByteSource::N8),
            0b0010_0110 => Self::Load(R8::H, ByteSource::N8),
            0b0010_1110 => Self::Load(R8::L, ByteSource::N8),
            0b0011_0110 => Self::Load(R8::HLInd, ByteSource::N8),
            0b0011_1110 => Self::Load(R8::A, ByteSource::N8),
            // rlca
            0b0000_0111 => Self::RotateLeft(R8::A),
            // rrca
            0b0000_1111 => Self::RotateRight(R8::A),
            // rla
            0b0001_0111 => Self::RotateLeftThruCarry(R8::A),
            // rra
            0b0001_1111 => Self::RotateRightThruCarry(R8::A),
            // daa
            0b0010_0111 => Self::DecimalAdjustAcc,
            // cpl
            0b0010_1111 => Self::ComplementAcc,
            // scf
            0b0011_0111 => Self::SetCarryFlag,
            // ccf
            0b0011_1111 => Self::ClearCarryFlag,
            // jr imm8
            0b0001_1000 => Self::JumpRelative(Condition::Always),
            // jr cond, imm8
            0b0010_0000 => Self::JumpRelative(Condition::NotZero),
            0b0010_1000 => Self::JumpRelative(Condition::Zero),
            0b0011_0000 => Self::JumpRelative(Condition::NotCarry),
            0b0011_1000 => Self::JumpRelative(Condition::Carry),
            // stop
            0b0001_0000 => Self::Stop,
            // BLOCK 1 : 8-bit register-to-register loads
            // ld r8, r8
            0x40 => Self::Load(R8::B, ByteSource::R8(R8::B)),
            0x41 => Self::Load(R8::B, ByteSource::R8(R8::C)),
            0x42 => Self::Load(R8::B, ByteSource::R8(R8::D)),
            0x43 => Self::Load(R8::B, ByteSource::R8(R8::E)),
            0x44 => Self::Load(R8::B, ByteSource::R8(R8::H)),
            0x45 => Self::Load(R8::B, ByteSource::R8(R8::L)),
            0x46 => Self::Load(R8::B, ByteSource::R8(R8::HLInd)),
            0x47 => Self::Load(R8::B, ByteSource::R8(R8::A)),

            0x48 => Self::Load(R8::C, ByteSource::R8(R8::B)),
            0x49 => Self::Load(R8::C, ByteSource::R8(R8::C)),
            0x4A => Self::Load(R8::C, ByteSource::R8(R8::D)),
            0x4B => Self::Load(R8::C, ByteSource::R8(R8::E)),
            0x4C => Self::Load(R8::C, ByteSource::R8(R8::H)),
            0x4D => Self::Load(R8::C, ByteSource::R8(R8::L)),
            0x4E => Self::Load(R8::C, ByteSource::R8(R8::HLInd)),
            0x4F => Self::Load(R8::C, ByteSource::R8(R8::A)),

            0x50 => Self::Load(R8::D, ByteSource::R8(R8::B)),
            0x51 => Self::Load(R8::D, ByteSource::R8(R8::C)),
            0x52 => Self::Load(R8::D, ByteSource::R8(R8::D)),
            0x53 => Self::Load(R8::D, ByteSource::R8(R8::E)),
            0x54 => Self::Load(R8::D, ByteSource::R8(R8::H)),
            0x55 => Self::Load(R8::D, ByteSource::R8(R8::L)),
            0x56 => Self::Load(R8::D, ByteSource::R8(R8::HLInd)),
            0x57 => Self::Load(R8::D, ByteSource::R8(R8::A)),

            0x58 => Self::Load(R8::E, ByteSource::R8(R8::B)),
            0x59 => Self::Load(R8::E, ByteSource::R8(R8::C)),
            0x5A => Self::Load(R8::E, ByteSource::R8(R8::D)),
            0x5B => Self::Load(R8::E, ByteSource::R8(R8::E)),
            0x5C => Self::Load(R8::E, ByteSource::R8(R8::H)),
            0x5D => Self::Load(R8::E, ByteSource::R8(R8::L)),
            0x5E => Self::Load(R8::E, ByteSource::R8(R8::HLInd)),
            0x5F => Self::Load(R8::E, ByteSource::R8(R8::A)),

            0x60 => Self::Load(R8::H, ByteSource::R8(R8::B)),
            0x61 => Self::Load(R8::H, ByteSource::R8(R8::C)),
            0x62 => Self::Load(R8::H, ByteSource::R8(R8::D)),
            0x63 => Self::Load(R8::H, ByteSource::R8(R8::E)),
            0x64 => Self::Load(R8::H, ByteSource::R8(R8::H)),
            0x65 => Self::Load(R8::H, ByteSource::R8(R8::L)),
            0x66 => Self::Load(R8::H, ByteSource::R8(R8::HLInd)),
            0x67 => Self::Load(R8::H, ByteSource::R8(R8::A)),

            0x68 => Self::Load(R8::L, ByteSource::R8(R8::B)),
            0x69 => Self::Load(R8::L, ByteSource::R8(R8::C)),
            0x6A => Self::Load(R8::L, ByteSource::R8(R8::D)),
            0x6B => Self::Load(R8::L, ByteSource::R8(R8::E)),
            0x6C => Self::Load(R8::L, ByteSource::R8(R8::H)),
            0x6D => Self::Load(R8::L, ByteSource::R8(R8::L)),
            0x6E => Self::Load(R8::L, ByteSource::R8(R8::HLInd)),
            0x6F => Self::Load(R8::L, ByteSource::R8(R8::A)),

            0x70 => Self::Load(R8::HLInd, ByteSource::R8(R8::B)),
            0x71 => Self::Load(R8::HLInd, ByteSource::R8(R8::C)),
            0x72 => Self::Load(R8::HLInd, ByteSource::R8(R8::D)),
            0x73 => Self::Load(R8::HLInd, ByteSource::R8(R8::E)),
            0x74 => Self::Load(R8::HLInd, ByteSource::R8(R8::H)),
            0x75 => Self::Load(R8::HLInd, ByteSource::R8(R8::L)),
            // exception!  halt
            0x76 => Self::Halt,
            0x77 => Self::Load(R8::HLInd, ByteSource::R8(R8::A)),

            0x78 => Self::Load(R8::A, ByteSource::R8(R8::B)),
            0x79 => Self::Load(R8::A, ByteSource::R8(R8::C)),
            0x7A => Self::Load(R8::A, ByteSource::R8(R8::D)),
            0x7B => Self::Load(R8::A, ByteSource::R8(R8::E)),
            0x7C => Self::Load(R8::A, ByteSource::R8(R8::H)),
            0x7D => Self::Load(R8::A, ByteSource::R8(R8::L)),
            0x7E => Self::Load(R8::A, ByteSource::R8(R8::HLInd)),
            0x7F => Self::Load(R8::A, ByteSource::R8(R8::A)),
            // BLOCK 2 : 8-bit arithmetic
            // add a, r8
            0x80 => Self::Add(ByteSource::R8(R8::B)),
            0x81 => Self::Add(ByteSource::R8(R8::C)),
            0x82 => Self::Add(ByteSource::R8(R8::D)),
            0x83 => Self::Add(ByteSource::R8(R8::E)),
            0x84 => Self::Add(ByteSource::R8(R8::H)),
            0x85 => Self::Add(ByteSource::R8(R8::L)),
            0x86 => Self::Add(ByteSource::R8(R8::HLInd)),
            0x87 => Self::Add(ByteSource::R8(R8::A)),
            // adc a, r8
            0x88 => Self::AddWithCarry(ByteSource::R8(R8::B)),
            0x89 => Self::AddWithCarry(ByteSource::R8(R8::C)),
            0x8A => Self::AddWithCarry(ByteSource::R8(R8::D)),
            0x8B => Self::AddWithCarry(ByteSource::R8(R8::E)),
            0x8C => Self::AddWithCarry(ByteSource::R8(R8::H)),
            0x8D => Self::AddWithCarry(ByteSource::R8(R8::L)),
            0x8E => Self::AddWithCarry(ByteSource::R8(R8::HLInd)),
            0x8F => Self::AddWithCarry(ByteSource::R8(R8::A)),
            // sub a, r8
            0x90 => Self::Sub(ByteSource::R8(R8::B)),
            0x91 => Self::Sub(ByteSource::R8(R8::C)),
            0x92 => Self::Sub(ByteSource::R8(R8::D)),
            0x93 => Self::Sub(ByteSource::R8(R8::E)),
            0x94 => Self::Sub(ByteSource::R8(R8::H)),
            0x95 => Self::Sub(ByteSource::R8(R8::L)),
            0x96 => Self::Sub(ByteSource::R8(R8::HLInd)),
            0x97 => Self::Sub(ByteSource::R8(R8::A)),
            // sub a, r8
            0x98 => Self::SubWithCarry(ByteSource::R8(R8::B)),
            0x99 => Self::SubWithCarry(ByteSource::R8(R8::C)),
            0x9A => Self::SubWithCarry(ByteSource::R8(R8::D)),
            0x9B => Self::SubWithCarry(ByteSource::R8(R8::E)),
            0x9C => Self::SubWithCarry(ByteSource::R8(R8::H)),
            0x9D => Self::SubWithCarry(ByteSource::R8(R8::L)),
            0x9E => Self::SubWithCarry(ByteSource::R8(R8::HLInd)),
            0x9F => Self::SubWithCarry(ByteSource::R8(R8::A)),
            // and a, r8
            0xA0 => Self::And(ByteSource::R8(R8::B)),
            0xA1 => Self::And(ByteSource::R8(R8::C)),
            0xA2 => Self::And(ByteSource::R8(R8::D)),
            0xA3 => Self::And(ByteSource::R8(R8::E)),
            0xA4 => Self::And(ByteSource::R8(R8::H)),
            0xA5 => Self::And(ByteSource::R8(R8::L)),
            0xA6 => Self::And(ByteSource::R8(R8::HLInd)),
            0xA7 => Self::And(ByteSource::R8(R8::A)),
            // xor a, r8
            0xA8 => Self::Xor(ByteSource::R8(R8::B)),
            0xA9 => Self::Xor(ByteSource::R8(R8::C)),
            0xAA => Self::Xor(ByteSource::R8(R8::D)),
            0xAB => Self::Xor(ByteSource::R8(R8::E)),
            0xAC => Self::Xor(ByteSource::R8(R8::H)),
            0xAD => Self::Xor(ByteSource::R8(R8::L)),
            0xAE => Self::Xor(ByteSource::R8(R8::HLInd)),
            0xAF => Self::Xor(ByteSource::R8(R8::A)),
            // or a, r8
            0xB0 => Self::Or(ByteSource::R8(R8::B)),
            0xB1 => Self::Or(ByteSource::R8(R8::C)),
            0xB2 => Self::Or(ByteSource::R8(R8::D)),
            0xB3 => Self::Or(ByteSource::R8(R8::E)),
            0xB4 => Self::Or(ByteSource::R8(R8::H)),
            0xB5 => Self::Or(ByteSource::R8(R8::L)),
            0xB6 => Self::Or(ByteSource::R8(R8::HLInd)),
            0xB7 => Self::Or(ByteSource::R8(R8::A)),
            // cp a, r8
            0xB8 => Self::Cmp(ByteSource::R8(R8::B)),
            0xB9 => Self::Cmp(ByteSource::R8(R8::C)),
            0xBA => Self::Cmp(ByteSource::R8(R8::D)),
            0xBB => Self::Cmp(ByteSource::R8(R8::E)),
            0xBC => Self::Cmp(ByteSource::R8(R8::H)),
            0xBD => Self::Cmp(ByteSource::R8(R8::L)),
            0xBE => Self::Cmp(ByteSource::R8(R8::HLInd)),
            0xBF => Self::Cmp(ByteSource::R8(R8::A)),
            // BLOCK 3
            // add a, imm8
            0b1100_0110 => Self::Add(ByteSource::N8),
            // adc a, imm8
            0b1100_1110 => Self::AddWithCarry(ByteSource::N8),
            // sub a, imm8
            0b1101_0110 => Self::Sub(ByteSource::N8),
            // sbc a, imm8
            0b1101_1110 => Self::SubWithCarry(ByteSource::N8),
            // and a, imm8
            0b1110_0110 => Self::And(ByteSource::N8),
            // xor a, imm8
            0b1110_1110 => Self::Xor(ByteSource::N8),
            // or a, imm8
            0b1111_0110 => Self::Or(ByteSource::N8),
            // cp a, imm8
            0b1111_1110 => Self::Cmp(ByteSource::N8),

            // ret cond
            0b1100_0000 => Self::Ret(Condition::NotZero),
            0b1100_1000 => Self::Ret(Condition::Zero),
            0b1101_0000 => Self::Ret(Condition::NotCarry),
            0b1101_1000 => Self::Ret(Condition::Carry),
            // ret
            0b1100_1001 => Self::Ret(Condition::Always),
            // reti
            0b1101_1001 => Self::RetInterrupt,
            // jp cond, imm16
            0b1100_0010 => Self::JumpAbsolute(Condition::Always, JumpSource::N16),
            0b1100_1010 => Self::JumpAbsolute(Condition::Zero, JumpSource::N16),
            0b1101_0010 => Self::JumpAbsolute(Condition::NotCarry, JumpSource::N16),
            0b1101_1010 => Self::JumpAbsolute(Condition::Carry, JumpSource::N16),
            // jp imm16
            0b1100_0011 => Self::JumpAbsolute(Condition::Always, JumpSource::N16),
            // jp hl
            0b1110_1001 => Self::JumpAbsolute(Condition::Always, JumpSource::HL),
            // call cond, imm16
            0b1100_0100 => Self::Call(Condition::NotZero),
            0b1100_1100 => Self::Call(Condition::Zero),
            0b1101_0100 => Self::Call(Condition::NotCarry),
            0b1101_1100 => Self::Call(Condition::Carry),
            // call  imm16
            0b1100_1101 => Self::Call(Condition::Always),
            // rst tgt3
            0b1100_0111 => Self::CallRST(RSTAddress::X00),
            0b1100_1111 => Self::CallRST(RSTAddress::X08),
            0b1101_0111 => Self::CallRST(RSTAddress::X10),
            0b1101_1111 => Self::CallRST(RSTAddress::X10),
            0b1110_0111 => Self::CallRST(RSTAddress::X28),
            0b1110_1111 => Self::CallRST(RSTAddress::X20),
            0b1111_0111 => Self::CallRST(RSTAddress::X38),
            0b1111_1111 => Self::CallRST(RSTAddress::X30),

            // pop r16stk
            0xC1 => Self::Pop(R16Stack::BC),
            0xD1 => Self::Pop(R16Stack::DE),
            0xE1 => Self::Pop(R16Stack::HL),
            0xF1 => Self::Pop(R16Stack::AF),
            // push r16stk
            0xC5 => Self::Push(R16Stack::BC),
            0xD5 => Self::Push(R16Stack::DE),
            0xE5 => Self::Push(R16Stack::HL),
            0xF5 => Self::Push(R16Stack::AF),

            // ldh [c], a
            0b1110_0010 => Self::LoadHalfAccToIndirect(LoadHalfTarget::C),
            // ldh [imm8], a
            0b1110_0000 => Self::LoadHalfAccToIndirect(LoadHalfTarget::N16),
            // ld [imm16], a
            0b1110_1010 => Self::LoadAccToIndirect(R16Mem::N16),
            // ldh a, [c]
            0b1111_0010 => Self::LoadHalfIndirectToAcc(LoadHalfTarget::C),
            // ldh a, [imm8]
            0b1111_0000 => Self::LoadHalfIndirectToAcc(LoadHalfTarget::N16),
            // ld a, [imm16]
            0b1111_1010 => Self::LoadIndirectToAcc(R16Mem::N16),
            // add sp, imm8
            0b1110_1000 => Self::AddSP,
            // ld hl, sp + imm8
            0b1111_1000 => Self::LoadSPPlusImmediateToHL,
            // ld sp, hl
            0b1111_1001 => Self::LoadHLToSP,

            // di
            0b1111_0011 => Self::DisableInterrupt,
            // ei
            0b1111_1011 => Self::EnableInterrupt,
            0xCB => {
                panic!("Trying to decode opcode prefix 0xCB")
            }
            0xD3 | 0xDB | 0xDD | 0xE3 | 0xE4 | 0xEB | 0xEC | 0xED | 0xF4 | 0xFC | 0xFD => {
                panic!("invalid opcode {:0x}", opcode)
            }
        }
    }
    pub(crate) fn from_0xCB_prefixed_opcode(opcode: u8) -> Self {
        match opcode {
            // rlc r8
            0x00 => Self::RotateLeft(R8::B),
            0x01 => Self::RotateLeft(R8::C),
            0x02 => Self::RotateLeft(R8::D),
            0x03 => Self::RotateLeft(R8::E),
            0x04 => Self::RotateLeft(R8::H),
            0x05 => Self::RotateLeft(R8::L),
            0x06 => Self::RotateLeft(R8::HLInd),
            0x07 => Self::RotateLeft(R8::A),
            // rrc r8
            0x08 => Self::RotateRight(R8::B),
            0x09 => Self::RotateRight(R8::C),
            0x0A => Self::RotateRight(R8::D),
            0x0B => Self::RotateRight(R8::E),
            0x0C => Self::RotateRight(R8::H),
            0x0D => Self::RotateRight(R8::L),
            0x0E => Self::RotateRight(R8::HLInd),
            0x0F => Self::RotateRight(R8::A),
            // rl r8
            0x10 => Self::RotateLeftThruCarry(R8::B),
            0x11 => Self::RotateLeftThruCarry(R8::C),
            0x12 => Self::RotateLeftThruCarry(R8::D),
            0x13 => Self::RotateLeftThruCarry(R8::E),
            0x14 => Self::RotateLeftThruCarry(R8::H),
            0x15 => Self::RotateLeftThruCarry(R8::L),
            0x16 => Self::RotateLeftThruCarry(R8::HLInd),
            0x17 => Self::RotateLeftThruCarry(R8::A),
            // rr r8
            0x18 => Self::RotateRightThruCarry(R8::B),
            0x19 => Self::RotateRightThruCarry(R8::C),
            0x1A => Self::RotateRightThruCarry(R8::D),
            0x1B => Self::RotateRightThruCarry(R8::E),
            0x1C => Self::RotateRightThruCarry(R8::H),
            0x1D => Self::RotateRightThruCarry(R8::L),
            0x1E => Self::RotateRightThruCarry(R8::HLInd),
            0x1F => Self::RotateRightThruCarry(R8::A),
            // sla r8
            0x20 => Self::ShiftLeftArithmetic(R8::B),
            0x21 => Self::ShiftLeftArithmetic(R8::C),
            0x22 => Self::ShiftLeftArithmetic(R8::D),
            0x23 => Self::ShiftLeftArithmetic(R8::E),
            0x24 => Self::ShiftLeftArithmetic(R8::H),
            0x25 => Self::ShiftLeftArithmetic(R8::L),
            0x26 => Self::ShiftLeftArithmetic(R8::HLInd),
            0x27 => Self::ShiftLeftArithmetic(R8::A),
            // sra r8
            0x28 => Self::ShiftRightArithmetic(R8::B),
            0x29 => Self::ShiftRightArithmetic(R8::C),
            0x2A => Self::ShiftRightArithmetic(R8::D),
            0x2B => Self::ShiftRightArithmetic(R8::E),
            0x2C => Self::ShiftRightArithmetic(R8::H),
            0x2D => Self::ShiftRightArithmetic(R8::L),
            0x2E => Self::ShiftRightArithmetic(R8::HLInd),
            0x2F => Self::ShiftRightArithmetic(R8::A),
            // swap r8
            0x30 => Self::Swap(R8::B),
            0x31 => Self::Swap(R8::C),
            0x32 => Self::Swap(R8::D),
            0x33 => Self::Swap(R8::E),
            0x34 => Self::Swap(R8::H),
            0x35 => Self::Swap(R8::L),
            0x36 => Self::Swap(R8::HLInd),
            0x37 => Self::Swap(R8::A),
            // srl r8
            0x38 => Self::ShiftRightLogical(R8::B),
            0x39 => Self::ShiftRightLogical(R8::C),
            0x3A => Self::ShiftRightLogical(R8::D),
            0x3B => Self::ShiftRightLogical(R8::E),
            0x3C => Self::ShiftRightLogical(R8::H),
            0x3D => Self::ShiftRightLogical(R8::L),
            0x3E => Self::ShiftRightLogical(R8::HLInd),
            0x3F => Self::ShiftRightLogical(R8::A),

            // bit b3, r8
            0x40 => Self::Bit(BitIndex::B0, R8::B),
            0x41 => Self::Bit(BitIndex::B0, R8::C),
            0x42 => Self::Bit(BitIndex::B0, R8::D),
            0x43 => Self::Bit(BitIndex::B0, R8::E),
            0x44 => Self::Bit(BitIndex::B0, R8::H),
            0x45 => Self::Bit(BitIndex::B0, R8::L),
            0x46 => Self::Bit(BitIndex::B0, R8::HLInd),
            0x47 => Self::Bit(BitIndex::B0, R8::A),

            0x48 => Self::Bit(BitIndex::B1, R8::B),
            0x49 => Self::Bit(BitIndex::B1, R8::C),
            0x4A => Self::Bit(BitIndex::B1, R8::D),
            0x4B => Self::Bit(BitIndex::B1, R8::E),
            0x4C => Self::Bit(BitIndex::B1, R8::H),
            0x4D => Self::Bit(BitIndex::B1, R8::L),
            0x4E => Self::Bit(BitIndex::B1, R8::HLInd),
            0x4F => Self::Bit(BitIndex::B1, R8::A),

            0x50 => Self::Bit(BitIndex::B2, R8::B),
            0x51 => Self::Bit(BitIndex::B2, R8::C),
            0x52 => Self::Bit(BitIndex::B2, R8::D),
            0x53 => Self::Bit(BitIndex::B2, R8::E),
            0x54 => Self::Bit(BitIndex::B2, R8::H),
            0x55 => Self::Bit(BitIndex::B2, R8::L),
            0x56 => Self::Bit(BitIndex::B2, R8::HLInd),
            0x57 => Self::Bit(BitIndex::B2, R8::A),

            0x58 => Self::Bit(BitIndex::B3, R8::B),
            0x59 => Self::Bit(BitIndex::B3, R8::C),
            0x5A => Self::Bit(BitIndex::B3, R8::D),
            0x5B => Self::Bit(BitIndex::B3, R8::E),
            0x5C => Self::Bit(BitIndex::B3, R8::H),
            0x5D => Self::Bit(BitIndex::B3, R8::L),
            0x5E => Self::Bit(BitIndex::B3, R8::HLInd),
            0x5F => Self::Bit(BitIndex::B3, R8::A),

            0x60 => Self::Bit(BitIndex::B4, R8::B),
            0x61 => Self::Bit(BitIndex::B4, R8::C),
            0x62 => Self::Bit(BitIndex::B4, R8::D),
            0x63 => Self::Bit(BitIndex::B4, R8::E),
            0x64 => Self::Bit(BitIndex::B4, R8::H),
            0x65 => Self::Bit(BitIndex::B4, R8::L),
            0x66 => Self::Bit(BitIndex::B4, R8::HLInd),
            0x67 => Self::Bit(BitIndex::B4, R8::A),

            0x68 => Self::Bit(BitIndex::B5, R8::B),
            0x69 => Self::Bit(BitIndex::B5, R8::C),
            0x6A => Self::Bit(BitIndex::B5, R8::D),
            0x6B => Self::Bit(BitIndex::B5, R8::E),
            0x6C => Self::Bit(BitIndex::B5, R8::H),
            0x6D => Self::Bit(BitIndex::B5, R8::L),
            0x6E => Self::Bit(BitIndex::B5, R8::HLInd),
            0x6F => Self::Bit(BitIndex::B5, R8::A),

            0x70 => Self::Bit(BitIndex::B6, R8::B),
            0x71 => Self::Bit(BitIndex::B6, R8::C),
            0x72 => Self::Bit(BitIndex::B6, R8::D),
            0x73 => Self::Bit(BitIndex::B6, R8::E),
            0x74 => Self::Bit(BitIndex::B6, R8::H),
            0x75 => Self::Bit(BitIndex::B6, R8::L),
            0x76 => Self::Bit(BitIndex::B6, R8::HLInd),
            0x77 => Self::Bit(BitIndex::B6, R8::A),

            0x78 => Self::Bit(BitIndex::B7, R8::B),
            0x79 => Self::Bit(BitIndex::B7, R8::C),
            0x7A => Self::Bit(BitIndex::B7, R8::D),
            0x7B => Self::Bit(BitIndex::B7, R8::E),
            0x7C => Self::Bit(BitIndex::B7, R8::H),
            0x7D => Self::Bit(BitIndex::B7, R8::L),
            0x7E => Self::Bit(BitIndex::B7, R8::HLInd),
            0x7F => Self::Bit(BitIndex::B7, R8::A),

            // res b3, r8
            0x80 => Self::ResetBit(BitIndex::B0, R8::B),
            0x81 => Self::ResetBit(BitIndex::B0, R8::C),
            0x82 => Self::ResetBit(BitIndex::B0, R8::D),
            0x83 => Self::ResetBit(BitIndex::B0, R8::E),
            0x84 => Self::ResetBit(BitIndex::B0, R8::H),
            0x85 => Self::ResetBit(BitIndex::B0, R8::L),
            0x86 => Self::ResetBit(BitIndex::B0, R8::HLInd),
            0x87 => Self::ResetBit(BitIndex::B0, R8::A),

            0x88 => Self::ResetBit(BitIndex::B1, R8::B),
            0x89 => Self::ResetBit(BitIndex::B1, R8::C),
            0x8A => Self::ResetBit(BitIndex::B1, R8::D),
            0x8B => Self::ResetBit(BitIndex::B1, R8::E),
            0x8C => Self::ResetBit(BitIndex::B1, R8::H),
            0x8D => Self::ResetBit(BitIndex::B1, R8::L),
            0x8E => Self::ResetBit(BitIndex::B1, R8::HLInd),
            0x8F => Self::ResetBit(BitIndex::B1, R8::A),

            0x90 => Self::ResetBit(BitIndex::B2, R8::B),
            0x91 => Self::ResetBit(BitIndex::B2, R8::C),
            0x92 => Self::ResetBit(BitIndex::B2, R8::D),
            0x93 => Self::ResetBit(BitIndex::B2, R8::E),
            0x94 => Self::ResetBit(BitIndex::B2, R8::H),
            0x95 => Self::ResetBit(BitIndex::B2, R8::L),
            0x96 => Self::ResetBit(BitIndex::B2, R8::HLInd),
            0x97 => Self::ResetBit(BitIndex::B2, R8::A),

            0x98 => Self::ResetBit(BitIndex::B3, R8::B),
            0x99 => Self::ResetBit(BitIndex::B3, R8::C),
            0x9A => Self::ResetBit(BitIndex::B3, R8::D),
            0x9B => Self::ResetBit(BitIndex::B3, R8::E),
            0x9C => Self::ResetBit(BitIndex::B3, R8::H),
            0x9D => Self::ResetBit(BitIndex::B3, R8::L),
            0x9E => Self::ResetBit(BitIndex::B3, R8::HLInd),
            0x9F => Self::ResetBit(BitIndex::B3, R8::A),

            0xA0 => Self::ResetBit(BitIndex::B4, R8::B),
            0xA1 => Self::ResetBit(BitIndex::B4, R8::C),
            0xA2 => Self::ResetBit(BitIndex::B4, R8::D),
            0xA3 => Self::ResetBit(BitIndex::B4, R8::E),
            0xA4 => Self::ResetBit(BitIndex::B4, R8::H),
            0xA5 => Self::ResetBit(BitIndex::B4, R8::L),
            0xA6 => Self::ResetBit(BitIndex::B4, R8::HLInd),
            0xA7 => Self::ResetBit(BitIndex::B4, R8::A),

            0xA8 => Self::ResetBit(BitIndex::B5, R8::B),
            0xA9 => Self::ResetBit(BitIndex::B5, R8::C),
            0xAA => Self::ResetBit(BitIndex::B5, R8::D),
            0xAB => Self::ResetBit(BitIndex::B5, R8::E),
            0xAC => Self::ResetBit(BitIndex::B5, R8::H),
            0xAD => Self::ResetBit(BitIndex::B5, R8::L),
            0xAE => Self::ResetBit(BitIndex::B5, R8::HLInd),
            0xAF => Self::ResetBit(BitIndex::B5, R8::A),

            0xB0 => Self::ResetBit(BitIndex::B6, R8::B),
            0xB1 => Self::ResetBit(BitIndex::B6, R8::C),
            0xB2 => Self::ResetBit(BitIndex::B6, R8::D),
            0xB3 => Self::ResetBit(BitIndex::B6, R8::E),
            0xB4 => Self::ResetBit(BitIndex::B6, R8::H),
            0xB5 => Self::ResetBit(BitIndex::B6, R8::L),
            0xB6 => Self::ResetBit(BitIndex::B6, R8::HLInd),
            0xB7 => Self::ResetBit(BitIndex::B6, R8::A),

            0xB8 => Self::ResetBit(BitIndex::B7, R8::B),
            0xB9 => Self::ResetBit(BitIndex::B7, R8::C),
            0xBA => Self::ResetBit(BitIndex::B7, R8::D),
            0xBB => Self::ResetBit(BitIndex::B7, R8::E),
            0xBC => Self::ResetBit(BitIndex::B7, R8::H),
            0xBD => Self::ResetBit(BitIndex::B7, R8::L),
            0xBE => Self::ResetBit(BitIndex::B7, R8::HLInd),
            0xBF => Self::ResetBit(BitIndex::B7, R8::A),
            // set b3, r8
            0xC0 => Self::SetBit(BitIndex::B0, R8::B),
            0xC1 => Self::SetBit(BitIndex::B0, R8::C),
            0xC2 => Self::SetBit(BitIndex::B0, R8::D),
            0xC3 => Self::SetBit(BitIndex::B0, R8::E),
            0xC4 => Self::SetBit(BitIndex::B0, R8::H),
            0xC5 => Self::SetBit(BitIndex::B0, R8::L),
            0xC6 => Self::SetBit(BitIndex::B0, R8::HLInd),
            0xC7 => Self::SetBit(BitIndex::B0, R8::A),

            0xC8 => Self::SetBit(BitIndex::B1, R8::B),
            0xC9 => Self::SetBit(BitIndex::B1, R8::C),
            0xCA => Self::SetBit(BitIndex::B1, R8::D),
            0xCB => Self::SetBit(BitIndex::B1, R8::E),
            0xCC => Self::SetBit(BitIndex::B1, R8::H),
            0xCD => Self::SetBit(BitIndex::B1, R8::L),
            0xCE => Self::SetBit(BitIndex::B1, R8::HLInd),
            0xCF => Self::SetBit(BitIndex::B1, R8::A),

            0xD0 => Self::SetBit(BitIndex::B2, R8::B),
            0xD1 => Self::SetBit(BitIndex::B2, R8::C),
            0xD2 => Self::SetBit(BitIndex::B2, R8::D),
            0xD3 => Self::SetBit(BitIndex::B2, R8::E),
            0xD4 => Self::SetBit(BitIndex::B2, R8::H),
            0xD5 => Self::SetBit(BitIndex::B2, R8::L),
            0xD6 => Self::SetBit(BitIndex::B2, R8::HLInd),
            0xD7 => Self::SetBit(BitIndex::B2, R8::A),

            0xD8 => Self::SetBit(BitIndex::B3, R8::B),
            0xD9 => Self::SetBit(BitIndex::B3, R8::C),
            0xDA => Self::SetBit(BitIndex::B3, R8::D),
            0xDB => Self::SetBit(BitIndex::B3, R8::E),
            0xDC => Self::SetBit(BitIndex::B3, R8::H),
            0xDD => Self::SetBit(BitIndex::B3, R8::L),
            0xDE => Self::SetBit(BitIndex::B3, R8::HLInd),
            0xDF => Self::SetBit(BitIndex::B3, R8::A),

            0xE0 => Self::SetBit(BitIndex::B4, R8::B),
            0xE1 => Self::SetBit(BitIndex::B4, R8::C),
            0xE2 => Self::SetBit(BitIndex::B4, R8::D),
            0xE3 => Self::SetBit(BitIndex::B4, R8::E),
            0xE4 => Self::SetBit(BitIndex::B4, R8::H),
            0xE5 => Self::SetBit(BitIndex::B4, R8::L),
            0xE6 => Self::SetBit(BitIndex::B4, R8::HLInd),
            0xE7 => Self::SetBit(BitIndex::B4, R8::A),

            0xE8 => Self::SetBit(BitIndex::B5, R8::B),
            0xE9 => Self::SetBit(BitIndex::B5, R8::C),
            0xEA => Self::SetBit(BitIndex::B5, R8::D),
            0xEB => Self::SetBit(BitIndex::B5, R8::E),
            0xEC => Self::SetBit(BitIndex::B5, R8::H),
            0xED => Self::SetBit(BitIndex::B5, R8::L),
            0xEE => Self::SetBit(BitIndex::B5, R8::HLInd),
            0xEF => Self::SetBit(BitIndex::B5, R8::A),

            0xF0 => Self::SetBit(BitIndex::B6, R8::B),
            0xF1 => Self::SetBit(BitIndex::B6, R8::C),
            0xF2 => Self::SetBit(BitIndex::B6, R8::D),
            0xF3 => Self::SetBit(BitIndex::B6, R8::E),
            0xF4 => Self::SetBit(BitIndex::B6, R8::H),
            0xF5 => Self::SetBit(BitIndex::B6, R8::L),
            0xF6 => Self::SetBit(BitIndex::B6, R8::HLInd),
            0xF7 => Self::SetBit(BitIndex::B6, R8::A),

            0xF8 => Self::SetBit(BitIndex::B7, R8::B),
            0xF9 => Self::SetBit(BitIndex::B7, R8::C),
            0xFA => Self::SetBit(BitIndex::B7, R8::D),
            0xFB => Self::SetBit(BitIndex::B7, R8::E),
            0xFC => Self::SetBit(BitIndex::B7, R8::H),
            0xFD => Self::SetBit(BitIndex::B7, R8::L),
            0xFE => Self::SetBit(BitIndex::B7, R8::HLInd),
            0xFF => Self::SetBit(BitIndex::B7, R8::A),
        }
    }
}
