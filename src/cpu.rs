use crate::registers::Registers;
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Target {
    Register(Reg),
    IndirectHL,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Value {
    Register(Reg),
    IndirectHL,
    Immediate(u8),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Instruction {
    Add { value: Value, carry: bool },
    Sub { value: Value, carry: bool },
    Compare { value: Value },
    Inc { target: Target },
    Dec { target: Target },
    And { value: Value },
    Or { value: Value },
    Xor { value: Value },
    ComplementCarryFlag,
    SetCarryFlag,
    ComplementAccumulator,
    DecimalAdjustAccumulator,
    Inc16 { reg: Reg16 },
    Dec16 { reg: Reg16 },
    AddHL { reg: Reg16 },
    Swap { reg: Reg },
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Reg {
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
enum Reg16 {
    AF,
    BC,
    DE,
    HL,
}
#[derive(Debug, Clone, Copy)]
#[allow(clippy::upper_case_acronyms)]
struct CPU {
    regs: Registers,
}
impl CPU {
    fn execute(&mut self, instruction: Instruction) {
        match instruction {
            Instruction::Add { value, carry } => {
                let carry = if carry { self.regs.carry() as u8 } else { 0 };
                let value = match value {
                    Value::Register(reg) => self.reg(reg),
                    Value::IndirectHL => unimplemented!(),
                    Value::Immediate(n) => n,
                } + carry;
                self.add(value);
            }
            Instruction::Sub { value, carry } => {
                let carry = if carry { self.regs.carry() as u8 } else { 0 };
                let value = match value {
                    Value::Register(reg) => self.reg(reg),
                    Value::IndirectHL => unimplemented!(),
                    Value::Immediate(n) => n,
                } + carry;
                self.sub(value);
            }
            Instruction::Compare { value } => {
                let value = match value {
                    Value::Register(reg) => self.reg(reg),
                    Value::IndirectHL => unimplemented!(),
                    Value::Immediate(n) => n,
                };
                self.sub(value);
            }
            Instruction::Inc { target } => {
                match target {
                    Target::Register(reg) => self.inc(reg),
                    Target::IndirectHL => unimplemented!(),
                };
            }
            Instruction::Dec { target } => {
                match target {
                    Target::Register(reg) => self.dec(reg),
                    Target::IndirectHL => unimplemented!(),
                };
            }
            Instruction::And { value } => {
                let value = match value {
                    Value::Register(reg) => self.reg(reg),
                    Value::IndirectHL => unimplemented!(),
                    Value::Immediate(n) => n,
                };
                self.and(value);
            }
            Instruction::Or { value } => {
                let value = match value {
                    Value::Register(reg) => self.reg(reg),
                    Value::IndirectHL => unimplemented!(),
                    Value::Immediate(n) => n,
                };
                self.or(value);
            }
            Instruction::Xor { value } => {
                let value = match value {
                    Value::Register(reg) => self.reg(reg),
                    Value::IndirectHL => unimplemented!(),
                    Value::Immediate(n) => n,
                };
                self.xor(value);
            }
            Instruction::ComplementCarryFlag => self.ccf(),
            Instruction::SetCarryFlag => self.scf(),
            Instruction::ComplementAccumulator => self.cla(),
            Instruction::DecimalAdjustAccumulator => self.daa(),
            Instruction::Inc16 { reg } => self.inc16(reg),
            Instruction::Dec16 { reg } => self.dec16(reg),
            Instruction::AddHL { reg } => self.addhl(reg),
            Instruction::Swap { reg } => self.swap(reg),
        }
    }
    fn reg(&self, target: Reg) -> u8 {
        match target {
            Reg::A => self.regs.a,
            Reg::B => self.regs.b,
            Reg::C => self.regs.c,
            Reg::D => self.regs.d,
            Reg::E => self.regs.e,
            Reg::F => self.regs.f,
            Reg::H => self.regs.h,
            Reg::L => self.regs.l,
        }
    }
    fn set_reg(&mut self, target: Reg, value: u8) {
        match target {
            Reg::A => self.regs.a = value,
            Reg::B => self.regs.b = value,
            Reg::C => self.regs.c = value,
            Reg::D => self.regs.d = value,
            Reg::E => self.regs.e = value,
            Reg::F => self.regs.f = value,
            Reg::H => self.regs.h = value,
            Reg::L => self.regs.l = value,
        }
    }
    fn reg16(&self, target: Reg16) -> u16 {
        match target {
            Reg16::AF => self.regs.af(),
            Reg16::BC => self.regs.bc(),
            Reg16::DE => self.regs.de(),
            Reg16::HL => self.regs.hl(),
        }
    }
    fn set_reg16(&mut self, target: Reg16, value: u16) {
        match target {
            Reg16::AF => self.regs.set_af(value),
            Reg16::BC => self.regs.set_bc(value),
            Reg16::DE => self.regs.set_de(value),
            Reg16::HL => self.regs.set_hl(value),
        }
    }
    // 8 bit arithmetic
    fn add(&mut self, value: u8) {
        let a = self.regs.a;
        let (new_value, overflow) = a.overflowing_add(value);
        self.regs.set_zero(new_value == 0);
        self.regs.set_sub(false);
        self.regs.set_carry(overflow);
        // Half Carry is set if adding the lower nibbles of the value and register A
        // together result in a value bigger than 0xF. If the result is larger than 0xF
        // than the addition caused a carry from the lower nibble to the upper nibble.
        self.regs.set_half_carry((a & 0xF) + (value & 0xF) > 0xF);
        self.regs.a = new_value;
    }

    fn sub(&mut self, value: u8) {
        let a = self.regs.a;
        let (new_value, overflow) = a.overflowing_sub(value);
        self.regs.set_zero(new_value == 0);
        self.regs.set_sub(true);
        self.regs.set_carry(overflow);
        self.regs.set_half_carry((a & 0xF) < (value & 0xF));
        self.regs.a = new_value
    }

    fn inc(&mut self, reg: Reg) {
        let r = self.reg(reg);
        let new_value = r.wrapping_add(1);
        self.regs.set_zero(new_value == 0);
        self.regs.set_sub(false);
        self.regs.set_half_carry((r & 0xF) + 1 > 0xF);
        self.set_reg(reg, new_value);
    }
    fn dec(&mut self, reg: Reg) {
        let r = self.reg(reg);
        let new_value = r.wrapping_sub(1);
        self.regs.set_zero(new_value == 0);
        self.regs.set_sub(true);
        self.regs.set_half_carry((r & 0xF) == 0);
        self.set_reg(reg, new_value);
    }

    fn and(&mut self, value: u8) {
        let new_value = self.regs.a & value;
        self.regs.set_zero(new_value == 0);
        self.regs.set_sub(false);
        self.regs.set_carry(false);
        self.regs.set_half_carry(true);
        self.regs.a = new_value
    }
    fn or(&mut self, value: u8) {
        let new_value = self.regs.a | value;
        self.regs.set_zero(new_value == 0);
        self.regs.set_sub(false);
        self.regs.set_carry(false);
        self.regs.set_half_carry(false);
        self.regs.a = new_value
    }
    fn xor(&mut self, value: u8) {
        let new_value = self.regs.a ^ value;
        self.regs.set_zero(new_value == 0);
        self.regs.set_sub(false);
        self.regs.set_carry(false);
        self.regs.set_half_carry(false);
        self.regs.a = new_value
    }
    fn ccf(&mut self) {
        self.regs.set_sub(false);
        self.regs.set_half_carry(false);
        self.regs.set_carry(!self.regs.carry());
    }
    fn scf(&mut self) {
        self.regs.set_sub(false);
        self.regs.set_half_carry(false);
        self.regs.set_carry(true);
    }
    fn daa(&mut self) {
        // From https://github.com/mvdnes/rboy/blob/main/src/cpu.rs#L794
        let mut a = self.regs.a;
        let carry = self.regs.carry();
        let half_carry = self.regs.half_carry();
        let sub = self.regs.sub();
        let mut adjust = if carry { 0x60 } else { 0x00 };
        if half_carry {
            adjust |= 0x06;
        };
        if !sub {
            if a & 0x0F > 0x09 {
                adjust |= 0x06;
            };
            if a > 0x99 {
                adjust |= 0x60;
            };
            a = a.wrapping_add(adjust);
        } else {
            a = a.wrapping_sub(adjust);
        }

        self.regs.set_carry(adjust >= 0x60);
        self.regs.set_half_carry(false);
        self.regs.set_zero(a == 0);
        self.regs.a = a;
    }
    fn cla(&mut self) {
        self.regs.a = !self.regs.a;
        self.regs.set_sub(true);
        self.regs.set_half_carry(true);
    }

    // 16-bit arithmetic
    fn inc16(&mut self, reg: Reg16) {
        let value = self.reg16(reg);
        self.set_reg16(reg, value + 1);
    }
    fn dec16(&mut self, reg: Reg16) {
        let value = self.reg16(reg);
        self.set_reg16(reg, value - 1);
    }
    fn addhl(&mut self, reg: Reg16) {
        let value = self.reg16(reg);
        let hl = self.regs.hl();
        let (new_value, overflow) = hl.overflowing_add(value);
        self.regs.set_sub(false);
        self.regs.set_carry(overflow);
        self.regs.set_half_carry((hl & 0xF) + (value & 0xF) > 0xF);
        self.regs.set_hl(new_value);
    }
    // Misc
    fn swap(&mut self, reg: Reg) {
        let r = self.reg(reg);
        let upper = (r & 0xF0) >> 4;
        let lower = (r & 0x0F) << 4;
        let new_value = lower | upper;
        self.regs.set_zero(new_value == 0);
        self.regs.set_sub(false);
        self.regs.set_carry(false);
        self.regs.set_half_carry(false);
        self.set_reg(reg, new_value);
    }
}
