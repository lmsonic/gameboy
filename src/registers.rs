#[derive(Debug, Clone, Copy)]

pub(crate) struct Registers {
    pub(crate) a: u8,
    pub(crate) b: u8,
    pub(crate) c: u8,
    pub(crate) d: u8,
    pub(crate) e: u8,
    pub(crate) f: FlagsRegister,
    pub(crate) h: u8,
    pub(crate) l: u8,
}

impl Registers {
    pub(crate) fn af(&self) -> u16 {
        u16::from(self.a) << 8 | u16::from(u8::from(self.f))
    }
    pub(crate) fn set_af(&mut self, value: u16) {
        self.a = (value & 0xFF00 >> 8) as u8;
        self.f = FlagsRegister::from((value & 0x00FF) as u8);
    }
    pub(crate) fn bc(&self) -> u16 {
        u16::from(self.b) << 8 | u16::from(self.c)
    }
    pub(crate) fn set_bc(&mut self, value: u16) {
        self.b = (value & 0xFF00 >> 8) as u8;
        self.c = (value & 0x00FF) as u8;
    }
    pub(crate) fn de(&self) -> u16 {
        u16::from(self.d) << 8 | u16::from(self.e)
    }
    pub(crate) fn set_de(&mut self, value: u16) {
        self.d = (value & 0xFF00 >> 8) as u8;
        self.e = (value & 0x00FF) as u8;
    }
    pub(crate) fn hl(&self) -> u16 {
        u16::from(self.h) << 8 | u16::from(self.l)
    }
    pub(crate) fn set_hl(&mut self, value: u16) {
        self.h = (value & 0xFF00 >> 8) as u8;
        self.l = (value & 0x00FF) as u8;
    }
}

// ┌-> Carry
// ┌-+> Subtraction
// | |
// 1111 0000
// | |
// └-+> Zero
//  └-> Half Carry
#[derive(Debug, Clone, Copy)]
pub(crate) struct FlagsRegister {
    pub(crate) zero: bool,
    pub(crate) subtract: bool,
    pub(crate) half_carry: bool,
    pub(crate) carry: bool,
}

pub(crate) const ZERO_FLAG_BITPOS: i32 = 7;

pub(crate) const SUB_FLAG_BITPOS: i32 = 6;

pub(crate) const HALF_CARRY_FLAG_BITPOS: i32 = 5;

pub(crate) const CARRY_FLAG_BITPOS: i32 = 4;

impl From<FlagsRegister> for u8 {
    fn from(flag: FlagsRegister) -> Self {
        (if flag.zero { 1 } else { 0 }) << ZERO_FLAG_BITPOS
            | (if flag.subtract { 1 } else { 0 }) << SUB_FLAG_BITPOS
            | (if flag.half_carry { 1 } else { 0 }) << HALF_CARRY_FLAG_BITPOS
            | (if flag.carry { 1 } else { 0 }) << CARRY_FLAG_BITPOS
    }
}

impl From<u8> for FlagsRegister {
    fn from(byte: u8) -> Self {
        Self {
            zero: ((byte >> ZERO_FLAG_BITPOS) & 1) != 0,
            subtract: ((byte >> SUB_FLAG_BITPOS) & 1) != 0,
            half_carry: ((byte >> HALF_CARRY_FLAG_BITPOS) & 1) != 0,
            carry: ((byte >> CARRY_FLAG_BITPOS) & 1) != 0,
        }
    }
}
