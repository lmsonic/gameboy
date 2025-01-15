use instruction::{Instruction, JumpCondition, JumpTarget, Reg, Reg16, Target, Value};
use registers::Registers;

mod instruction;
mod registers;
#[derive(Debug, Clone)]
#[allow(clippy::upper_case_acronyms)]
struct CPU {
    regs: Registers,
    pc: u16,
    sp: u16,
    memory: Memory,
}
#[derive(Debug, Clone)]
struct Memory {
    memory: [u8; 65535],
}

impl Memory {
    fn read(&self, address: u16) -> u8 {
        self.memory[usize::from(address)]
    }
}
impl CPU {
    fn step(&mut self) {
        let byte = self.memory.read(self.pc);
        let instruction = if byte == 0xCB {
            self.pc += 1;
            Instruction::from_prefixed(self.memory.read(self.pc))
        } else {
            Instruction::from_opcode(byte)
        };
        self.pc = self.execute(instruction);
    }
    fn execute(&mut self, instruction: Instruction) -> u16 {
        match instruction {
            Instruction::Nop => {}
            Instruction::Stop => std::process::exit(0),
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
            Instruction::ComplementCarryFlag => self.complement_carry_flag(),
            Instruction::SetCarryFlag => self.set_carry_flag(),
            Instruction::ComplementAccumulator => self.complement_accumulator(),
            Instruction::DecimalAdjustAccumulator => self.decimal_adjust_accumulator(),
            Instruction::Inc16 { reg } => self.inc16(reg),
            Instruction::Dec16 { reg } => self.dec16(reg),
            Instruction::AddHL { reg } => self.addhl(reg),
            Instruction::Swap { reg } => self.swap(reg),
            Instruction::RotateRight { reg, through_carry } => {
                self.rotate_right(reg, through_carry);
            }
            Instruction::RotateLeft { reg, through_carry } => {
                self.rotate_left(reg, through_carry);
            }
            Instruction::ShiftRight { reg, set_msb_zero } => {
                self.shift_right(reg, set_msb_zero);
            }
            Instruction::ShiftLeft { reg } => self.shift_left(reg),
            Instruction::Bit { reg, bit } => self.bit(reg, bit),
            Instruction::SetBit { reg, bit } => self.set_bit(reg, bit),
            Instruction::ResetBit { reg, bit } => self.reset_bit(reg, bit),
            Instruction::Jump { condition, target } => self.jump(condition, target),
        }
        self.pc + 2
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
            Reg16::SP => self.sp,
        }
    }
    fn set_reg16(&mut self, target: Reg16, value: u16) {
        match target {
            Reg16::AF => self.regs.set_af(value),
            Reg16::BC => self.regs.set_bc(value),
            Reg16::DE => self.regs.set_de(value),
            Reg16::HL => self.regs.set_hl(value),
            Reg16::SP => self.sp = value,
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
    fn complement_carry_flag(&mut self) {
        self.regs.set_sub(false);
        self.regs.set_half_carry(false);
        self.regs.set_carry(!self.regs.carry());
    }
    fn set_carry_flag(&mut self) {
        self.regs.set_sub(false);
        self.regs.set_half_carry(false);
        self.regs.set_carry(true);
    }
    fn decimal_adjust_accumulator(&mut self) {
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
    fn complement_accumulator(&mut self) {
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
    /// Shift rotate operations flag updates
    fn sr_flag_update(&mut self, result: u8, carry: bool) {
        self.regs.set_zero(result == 0);
        self.regs.set_carry(carry);
        self.regs.set_sub(false);
        self.regs.set_half_carry(false);
    }
    fn rotate_left(&mut self, reg: Reg, through_carry: bool) {
        let r = self.reg(reg);
        let carry = (r & 0x80) == 0x80;
        let new_value = if through_carry {
            r << 1 | if self.regs.carry() { 1 } else { 0 }
        } else {
            r.rotate_left(1)
        };
        self.sr_flag_update(new_value, carry);
        self.set_reg(reg, new_value);
    }

    fn rotate_right(&mut self, reg: Reg, through_carry: bool) {
        let r = self.reg(reg);
        let carry = (r & 0x01) == 0x01;
        let new_value = if through_carry {
            r >> 1 | if self.regs.carry() { 0x80 } else { 0 }
        } else {
            r.rotate_left(1)
        };
        self.sr_flag_update(new_value, carry);
        self.set_reg(reg, new_value);
    }

    fn shift_left(&mut self, reg: Reg) {
        let r = self.reg(reg);
        let carry = (r & 0x80) == 0x80;
        let new_value = r << 1;
        self.sr_flag_update(new_value, carry);
        self.set_reg(reg, new_value);
    }
    fn shift_right(&mut self, reg: Reg, set_msb_zero: bool) {
        let r = self.reg(reg);
        let carry = (r & 0x01) == 0x01;
        let new_value = (r >> 1) | if set_msb_zero { 0 } else { r & 0x80 };
        self.sr_flag_update(new_value, carry);
        self.set_reg(reg, new_value);
    }
    fn bit(&mut self, reg: Reg, bit: u8) {
        let r = self.reg(reg);
        let z = (r >> bit) & 1;
        self.regs.set_zero(z == 0);
        self.regs.set_sub(false);
        self.regs.set_half_carry(true);
    }
    fn set_bit(&mut self, reg: Reg, bit: u8) {
        let r = self.reg(reg);
        self.set_reg(reg, r | (1 << bit));
    }
    fn reset_bit(&mut self, reg: Reg, bit: u8) {
        let r = self.reg(reg);
        self.set_reg(reg, r & !(1 << bit));
    }

    fn jump(&mut self, condition: JumpCondition, target: JumpTarget) {
        let address = match target {
            JumpTarget::HL => self.regs.hl(),
            JumpTarget::Absolute => {
                // Gameboy is little endian so read pc + 2 as most significant bit
                // and pc + 1 as least significant bit
                let least_significant_byte = self.memory.read(self.pc + 1) as u16;
                let most_significant_byte = self.memory.read(self.pc + 2) as u16;
                self.pc += 3;
                (most_significant_byte << 8) | least_significant_byte
            }
            JumpTarget::Relative => {
                let offset = i16::from(self.memory.read(self.pc + 1) as i8);
                self.pc += 2;
                (self.pc as i16 + offset) as u16
            }
        };
        if match condition {
            JumpCondition::NotZero => !self.regs.zero(),
            JumpCondition::Zero => self.regs.zero(),
            JumpCondition::NotCarry => !self.regs.carry(),
            JumpCondition::Carry => self.regs.carry(),
            JumpCondition::Always => true,
        } {
            self.pc = address;
        }
    }
}
