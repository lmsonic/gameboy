#[derive(Debug, Clone, Copy, Default)]

pub(crate) struct Registers {
    pub(crate) a: u8,
    pub(crate) b: u8,
    pub(crate) c: u8,
    pub(crate) d: u8,
    pub(crate) e: u8,
    pub(crate) f: u8,
    pub(crate) h: u8,
    pub(crate) l: u8,
}

const ZERO_FLAG_BITPOS: u32 = 7;
const NEG_FLAG_BITPOS: u32 = 6;
const HALF_CARRY_FLAG_BITPOS: u32 = 5;
const CARRY_FLAG_BITPOS: u32 = 4;
impl Registers {
    pub(crate) fn new() -> Self {
        Self {
            a: 0,
            b: 0,
            c: 0,
            d: 0,
            e: 0,
            f: 0,
            h: 0,
            l: 0,
        }
    }

    pub(crate) fn af(&self) -> u16 {
        u16::from(self.a) << 8 | u16::from(u8::from(self.f))
    }
    pub(crate) fn set_af(&mut self, value: u16) {
        self.a = (value & 0xFF00 >> 8) as u8;
        self.f = (value & 0x00FF) as u8;
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
    pub(crate) fn zero(&self) -> bool {
        ((self.f >> ZERO_FLAG_BITPOS) & 1) == 1
    }
    pub(crate) fn set_zero(&mut self, value: bool) {
        self.f |= if value { 1 } else { 0 } << ZERO_FLAG_BITPOS;
    }
    pub(crate) fn neg(&self) -> bool {
        ((self.f >> NEG_FLAG_BITPOS) & 1) == 1
    }
    pub(crate) fn set_neg(&mut self, value: bool) {
        self.f |= if value { 1 } else { 0 } << NEG_FLAG_BITPOS;
    }
    pub(crate) fn half_carry(&self) -> bool {
        ((self.f >> HALF_CARRY_FLAG_BITPOS) & 1) == 1
    }
    pub(crate) fn set_half_carry(&mut self, value: bool) {
        self.f |= if value { 1 } else { 0 } << HALF_CARRY_FLAG_BITPOS;
    }
    pub(crate) fn carry(&self) -> bool {
        ((self.f >> CARRY_FLAG_BITPOS) & 1) == 1
    }
    pub(crate) fn set_carry(&mut self, value: bool) {
        self.f |= if value { 1 } else { 0 } << CARRY_FLAG_BITPOS;
    }
}
