use crate::{
    instruction::{
        ByteSource, Condition, Instr, JumpSource, LoadHalfTarget, R16Mem, R16Stack, R16, R8,
    },
    registers::Registers,
};

#[derive(Debug, Clone)]
#[allow(clippy::upper_case_acronyms)]
struct CPU {
    regs: Registers,
    pc: u16,
    sp: u16,
    memory: [u8; 65535],
    is_halted: bool,
    interrupts_enabled: bool,
}

impl CPU {
    fn new() -> Self {
        Self {
            regs: Registers::new(),
            pc: 0,
            sp: 0,
            memory: [0; 65535],
            is_halted: false,
            interrupts_enabled: true,
        }
    }

    fn read(&self, address: u16) -> u8 {
        self.memory[usize::from(address)]
    }

    fn write(&mut self, address: u16, value: u8) {
        self.memory[usize::from(address)] = value
    }
    fn fetch(&mut self) -> u8 {
        let v = self.read(self.pc);
        self.pc += 1;
        v
    }
    fn fetch_word(&mut self) -> u16 {
        let lsb = u16::from(self.fetch());
        let msb = u16::from(self.fetch());
        (msb << 8) | lsb
    }
    fn read_word(&mut self, address: u16) -> u16 {
        let lsb = u16::from(address);
        let msb = u16::from(address + 1);
        (msb << 8) | lsb
    }
    fn step(&mut self) {
        let byte = self.read(self.pc);
        if !self.is_halted {
            self.pc += 1;
        }
        let instruction = if byte == 0xCB {
            let byte = self.fetch();
            Instr::from_0xCB_prefixed_opcode(byte)
        } else {
            Instr::from_opcode(byte)
        };

        self.execute(instruction);
    }
    fn reg(&self, r8: R8) -> u8 {
        match r8 {
            R8::B => self.regs.b,
            R8::C => self.regs.c,
            R8::D => self.regs.d,
            R8::E => self.regs.e,
            R8::H => self.regs.h,
            R8::L => self.regs.l,
            R8::HLInd => self.read(self.regs.hl()),
            R8::A => self.regs.a,
        }
    }

    fn set_reg(&mut self, r8: R8, value: u8) {
        match r8 {
            R8::B => self.regs.b = value,
            R8::C => self.regs.c = value,
            R8::D => self.regs.d = value,
            R8::E => self.regs.e = value,
            R8::H => self.regs.h = value,
            R8::L => self.regs.l = value,
            R8::HLInd => self.write(self.regs.hl(), value),
            R8::A => self.regs.a = value,
        }
    }
    fn reg16(&mut self, r16: R16) -> u16 {
        match r16 {
            R16::BC => self.regs.bc(),
            R16::DE => self.regs.de(),
            R16::HL => self.regs.hl(),
            R16::SP => self.sp,
        }
    }
    fn set_reg16(&mut self, r16: R16, value: u16) {
        match r16 {
            R16::BC => self.regs.set_bc(value),
            R16::DE => self.regs.set_de(value),
            R16::HL => self.regs.set_hl(value),
            R16::SP => self.sp = value,
        }
    }
    fn get_byte_source(&mut self, byte_source: ByteSource) -> u8 {
        match byte_source {
            ByteSource::R8(r8) => self.reg(r8),
            ByteSource::N8 => self.fetch(),
        }
    }
    fn check_condition(&self, condition: Condition) -> bool {
        match condition {
            Condition::NotZero => !self.regs.zero(),
            Condition::Zero => self.regs.zero(),
            Condition::NotCarry => !self.regs.carry(),
            Condition::Carry => self.regs.carry(),
            Condition::Always => true,
        }
    }

    fn execute(&mut self, instruction: Instr) {
        match instruction {
            Instr::Nop => {}
            Instr::Load(r8, byte_source) => {
                let value = self.get_byte_source(byte_source);
                self.set_reg(r8, value);
            }
            Instr::Load16Immediate(r16) => {
                let value = self.fetch_word();
                self.set_reg16(r16, value);
            }
            Instr::LoadIndirectToAcc(r16_mem) => {
                let address = match r16_mem {
                    R16Mem::BC => self.regs.bc(),
                    R16Mem::DE => self.regs.de(),
                    R16Mem::HLInc | R16Mem::HLDec => self.regs.hl(),
                    R16Mem::N16 => self.fetch_word(),
                };
                self.regs.a = self.read(address);
                match r16_mem {
                    R16Mem::HLInc => {
                        self.regs.set_hl(self.regs.hl() + 1);
                    }
                    R16Mem::HLDec => {
                        self.regs.set_hl(self.regs.hl() - 1);
                    }
                    _ => {}
                }
            }
            Instr::LoadAccToIndirect(r16_mem) => {
                let address = match r16_mem {
                    R16Mem::BC => self.regs.bc(),
                    R16Mem::DE => self.regs.de(),
                    R16Mem::HLInc | R16Mem::HLDec => self.regs.hl(),
                    R16Mem::N16 => self.fetch_word(),
                };
                self.write(address, self.regs.a);
                match r16_mem {
                    R16Mem::HLInc => {
                        self.regs.set_hl(self.regs.hl() + 1);
                    }
                    R16Mem::HLDec => {
                        self.regs.set_hl(self.regs.hl() - 1);
                    }
                    _ => {}
                }
            }
            Instr::LoadSPToIndirectImmediate => {
                let address = self.fetch_word();
                let lsb = (self.sp & 0x00FF) as u8;
                let msb = (self.sp >> 8) as u8;
                self.write(address, lsb);
                self.write(address + 1, msb);
            }
            Instr::LoadHalfIndirectToAcc(load_half_target) => {
                let address = match load_half_target {
                    LoadHalfTarget::C => 0xFF00 + u16::from(self.regs.c),
                    LoadHalfTarget::N16 => self.fetch_word(),
                };
                self.regs.a = self.read(address);
            }
            Instr::LoadHalfAccToIndirect(load_half_target) => {
                let address = match load_half_target {
                    LoadHalfTarget::C => 0xFF00 + u16::from(self.regs.c),
                    LoadHalfTarget::N16 => self.fetch_word(),
                };
                self.write(address, self.regs.a);
            }
            Instr::Add(byte_source, with_carry) => {
                let carry = if with_carry && self.regs.carry() {
                    1
                } else {
                    0
                };
                let old_a = self.regs.a;
                let value = self.get_byte_source(byte_source);
                let (new_value, overflow1) = old_a.overflowing_add(value);
                let (new_value, overflow2) = new_value.overflowing_add(carry);
                self.regs.a = new_value;

                self.regs.set_zero(new_value == 0);
                self.regs.set_neg(false);
                self.regs
                    .set_half_carry(((old_a & 0xF) + (value & 0xF)) + carry > 0xF);
                self.regs.set_carry(overflow1 || overflow2);
            }

            Instr::Sub(byte_source, with_carry) => {
                let carry = if with_carry && self.regs.carry() {
                    1
                } else {
                    0
                };
                let old_a = self.regs.a;
                let value = self.get_byte_source(byte_source);
                let (new_value, overflow1) = old_a.overflowing_sub(value);
                let (new_value, overflow2) = new_value.overflowing_sub(carry);
                self.regs.a = new_value;

                self.regs.set_zero(new_value == 0);
                self.regs.set_neg(true);
                self.regs
                    .set_half_carry((old_a & 0xF) < (value & 0xF) + carry);
                self.regs.set_carry(overflow1 || overflow2);
            }
            Instr::And(byte_source) => {
                let value = self.get_byte_source(byte_source);
                self.regs.a &= value;

                self.regs.set_zero(self.regs.a == 0);
                self.regs.set_neg(false);
                self.regs.set_half_carry(true);
                self.regs.set_carry(false);
            }
            Instr::Xor(byte_source) => {
                let value = self.get_byte_source(byte_source);
                self.regs.a ^= value;

                self.regs.set_zero(self.regs.a == 0);
                self.regs.set_neg(false);
                self.regs.set_half_carry(false);
                self.regs.set_carry(false);
            }
            Instr::Or(byte_source) => {
                let value = self.get_byte_source(byte_source);
                self.regs.a |= value;

                self.regs.set_zero(self.regs.a == 0);
                self.regs.set_neg(false);
                self.regs.set_half_carry(false);
                self.regs.set_carry(false);
            }
            Instr::Cmp(byte_source) => {
                let value = self.get_byte_source(byte_source);
                let (new_value, overflow) = self.regs.a.overflowing_sub(value);

                self.regs.set_zero(new_value == 0);
                self.regs.set_neg(true);
                self.regs
                    .set_half_carry((self.regs.a & 0xF) < (value & 0xF));
                self.regs.set_carry(overflow);
            }
            Instr::Inc(r8) => {
                let value = self.reg(r8);
                self.set_reg(r8, value.wrapping_add(1));

                self.regs.set_zero(self.reg(r8) == 0);
                self.regs.set_neg(false);
                self.regs.set_half_carry((value & 0xF) + 1 > 0xF);
            }
            Instr::Dec(r8) => {
                let value = self.reg(r8);

                self.set_reg(r8, value.wrapping_sub(1));

                self.regs.set_zero(self.reg(r8) == 0);
                self.regs.set_neg(true);
                self.regs.set_half_carry((value & 0xF) == 0);
            }
            Instr::AddHL(rhs) => {
                let value = self.reg16(rhs);
                let hl = self.regs.hl();
                let (new_value, overflow) = hl.overflowing_add(value);
                self.regs.set_hl(new_value);

                self.regs.set_neg(false);
                // Half carry tests if we flow over the 11th bit i.e. does adding the two
                // numbers together cause the 11th bit to flip
                let mask = 0b111_1111_1111; // mask out bits 11-15
                self.regs
                    .set_half_carry((value & mask) + (hl & mask) > mask);
                self.regs.set_carry(overflow);
            }
            Instr::Inc16(r16) => {
                let value = self.reg16(r16);
                self.set_reg16(r16, value.wrapping_add(1));
            }
            Instr::Dec16(r16) => {
                let value = self.reg16(r16);
                self.set_reg16(r16, value.wrapping_sub(1));
            }
            Instr::RotateLeft(r8, through_carry) => {
                let value = self.reg(r8);
                let carry = (value & 0x80) == 0x80;
                let new_value = if through_carry {
                    value << 1 | if self.regs.carry() { 1 } else { 0 }
                } else {
                    value.rotate_left(1)
                };
                self.set_reg(r8, new_value);

                self.regs
                    .set_zero(if r8 != R8::A { new_value == 0 } else { false });
                self.regs.set_neg(false);
                self.regs.set_half_carry(false);
                self.regs.set_carry(carry);
            }
            Instr::RotateRight(r8, through_carry) => {
                let value = self.reg(r8);
                let carry = (value & 0x01) == 0x01;
                let new_value = if through_carry {
                    value >> 1 | if self.regs.carry() { 0x80 } else { 0 }
                } else {
                    value.rotate_right(1)
                };
                self.set_reg(r8, new_value);

                self.regs
                    .set_zero(if r8 != R8::A { new_value == 0 } else { false });
                self.regs.set_neg(false);
                self.regs.set_half_carry(false);
                self.regs.set_carry(carry);
            }

            Instr::ShiftLeftArithmetic(r8) => {
                let value = self.reg(r8);
                let carry = (value & 0x80) == 0x80;
                let new_value = value << 1;
                self.set_reg(r8, new_value);

                self.regs.set_zero(new_value == 0);
                self.regs.set_neg(false);
                self.regs.set_half_carry(false);
                self.regs.set_carry(carry);
            }
            Instr::ShiftRightArithmetic(r8) => {
                let value = self.reg(r8);
                let carry = (value & 0x01) == 0x01;
                let new_value = (value >> 1) | value & 0x80;
                self.set_reg(r8, new_value);

                self.regs.set_zero(new_value == 0);
                self.regs.set_neg(false);
                self.regs.set_half_carry(false);
                self.regs.set_carry(carry);
            }
            Instr::ShiftRightLogical(r8) => {
                let value = self.reg(r8);
                let carry = (value & 0x01) == 0x01;
                let new_value = value >> 1;
                self.set_reg(r8, new_value);

                self.regs.set_zero(new_value == 0);
                self.regs.set_neg(false);
                self.regs.set_half_carry(false);
                self.regs.set_carry(carry);
            }
            Instr::Swap(r8) => {
                let value = self.reg(r8);
                let upper = (value & 0xF0) >> 4;
                let lower = value & 0x0F;
                let new_value = (lower << 4) | upper;
                self.set_reg(r8, new_value);

                self.regs.set_zero(new_value == 0);
                self.regs.set_neg(false);
                self.regs.set_half_carry(false);
                self.regs.set_carry(false);
            }
            Instr::Bit(bit_index, r8) => {
                let value = self.reg(r8);
                let bit = (value >> u8::from(bit_index)) & 1;

                self.regs.set_zero(bit == 0);
                self.regs.set_neg(false);
                self.regs.set_half_carry(true);
            }
            Instr::ResetBit(bit_index, r8) => {
                let value = self.reg(r8);
                let new_value = value & !(1 << u8::from(bit_index));
                self.set_reg(r8, new_value);
            }
            Instr::SetBit(bit_index, r8) => {
                let value = self.reg(r8);
                let new_value = value | (1 << u8::from(bit_index));
                self.set_reg(r8, new_value);
            }
            Instr::DecimalAdjustAcc => {
                let mut adj = 0;
                let n = self.regs.neg();
                let h = self.regs.half_carry();
                let c = self.regs.carry();
                let a = self.regs.a;
                if n {
                    if h {
                        adj += 0x06;
                    }
                    if c {
                        adj += 0x60;
                    }
                    self.regs.a -= adj;
                    self.regs.set_carry(adj > a);
                } else {
                    if h || (a & 0x0F) > 0x09 {
                        adj += 0x06;
                    }
                    if c || a > 0x9F {
                        adj += 0x60;
                    }
                    let (new_value, overflow) = self.regs.a.overflowing_add(adj);
                    self.regs.a = new_value;
                    self.regs.set_carry(overflow);
                }
                self.regs.set_zero(self.regs.a == 0);
                self.regs.set_half_carry(false);
            }
            Instr::ComplementAcc => {
                self.regs.a = !self.regs.a;
                self.regs.set_neg(true);
                self.regs.set_half_carry(true);
            }
            Instr::SetCarryFlag => {
                self.regs.set_neg(false);
                self.regs.set_half_carry(false);
                self.regs.set_carry(true);
            }
            Instr::ComplementCarryFlag => {
                self.regs.set_neg(false);
                self.regs.set_half_carry(false);
                self.regs.set_carry(!self.regs.carry());
            }
            Instr::JumpRelative(condition) => {
                let offset = self.fetch() as i8;
                if self.check_condition(condition) {
                    self.pc = self.pc.wrapping_add(offset as u16);
                }
            }
            Instr::JumpAbsolute(condition, jump_source) => {
                let address = match jump_source {
                    JumpSource::HL => self.regs.hl(),
                    JumpSource::N16 => self.fetch_word(),
                };

                if self.check_condition(condition) {
                    self.pc = address;
                }
            }
            Instr::Stop => {
                unimplemented!()
            }
            Instr::Halt => {
                self.is_halted = true;
            }
            Instr::Ret(condition) => {
                if self.check_condition(condition) {
                    self.pc = self.pop();
                }
                if condition == Condition::Always {
                    self.interrupts_enabled = true;
                }
            }
            Instr::RetInterrupt => {
                self.pc = self.pop();
                self.interrupts_enabled = true;
            }
            Instr::Call(condition) => {
                let address = self.fetch_word();
                if self.check_condition(condition) {
                    self.push(address);
                    self.pc = self.fetch_word();
                }
            }
            Instr::CallRST(rstaddress) => {
                let address = u16::from(rstaddress);
                self.push(address);
                self.pc = self.fetch_word();
            }
            Instr::Pop(r16_stack) => {
                let value = self.pop();
                match r16_stack {
                    R16Stack::BC => self.regs.set_bc(value),
                    R16Stack::DE => self.regs.set_de(value),
                    R16Stack::HL => self.regs.set_hl(value),
                    R16Stack::AF => self.regs.set_af(value),
                }
            }
            Instr::Push(r16_stack) => {
                let value = match r16_stack {
                    R16Stack::BC => self.regs.bc(),
                    R16Stack::DE => self.regs.de(),
                    R16Stack::HL => self.regs.hl(),
                    R16Stack::AF => self.regs.af(),
                };
                self.push(value);
            }
            Instr::AddSP => {
                let offset = i16::from(self.fetch() as i8);
                if offset > 0 {
                    self.sp += offset as u16;
                } else {
                    self.sp -= offset as u16;
                }
                self.regs.set_zero(false);
                self.regs.set_neg(false);
                self.regs
                    .set_half_carry((self.sp & 0xF) + (offset as u16 & 0x0F) > 0x0F);
                self.regs
                    .set_carry((self.sp & 0xFF) + (offset as u16 & 0xFF) > 0xFF);
            }
            Instr::LoadSPPlusImmediateToHL => {
                let offset = i16::from(self.fetch() as i8);
                let offsetted_sp = if offset > 0 {
                    self.sp + offset as u16
                } else {
                    self.sp - offset as u16
                };
                self.regs.set_hl(offsetted_sp);

                self.regs.set_zero(false);
                self.regs.set_neg(false);
                self.regs
                    .set_half_carry((offsetted_sp & 0xF) + (offset as u16 & 0x0F) > 0x0F);
                self.regs
                    .set_carry((offsetted_sp & 0xFF) + (offset as u16 & 0xFF) > 0xFF);
            }
            Instr::LoadHLToSP => {
                self.sp = self.regs.hl();
            }
            Instr::DisableInterrupt => self.interrupts_enabled = false,
            Instr::EnableInterrupt => self.interrupts_enabled = true,
        }
    }
    fn push(&mut self, value: u16) {
        self.sp = self.sp.wrapping_sub(1);

        let lsb = ((value & 0xFF00) >> 8) as u8;
        self.write(self.sp, lsb);
        self.sp = self.sp.wrapping_sub(1);
        let msb = (value & 0x00FF) as u8;
        self.write(self.sp, msb);
    }
    fn pop(&mut self) -> u16 {
        let lsb = u16::from(self.read(self.sp));
        self.sp = self.sp.wrapping_add(1);
        let msb = u16::from(self.read(self.sp));
        self.sp = self.sp.wrapping_add(1);
        (msb << 8) | lsb
    }
}
#[cfg(test)]
mod tests {
    use crate::instruction::BitIndex;

    use super::*;
    fn check_flags(cpu: &CPU, zero: bool, neg: bool, half_carry: bool, carry: bool) {
        assert_eq!(cpu.regs.zero(), zero);
        assert_eq!(cpu.regs.neg(), neg);
        assert_eq!(cpu.regs.half_carry(), half_carry);
        assert_eq!(cpu.regs.carry(), carry);
    }

    // INC
    #[test]
    fn execute_inc_8bit_non_overflow() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x7;
        cpu.execute(Instr::Inc(R8::A));

        assert_eq!(cpu.regs.a, 0x8);
        check_flags(&cpu, false, false, false, false);
    }

    #[test]
    fn execute_inc_8bit_half_carry() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0xF;
        cpu.execute(Instr::Inc(R8::A));

        assert_eq!(cpu.regs.a, 0x10);
        check_flags(&cpu, false, false, true, false);
    }

    #[test]
    fn execute_inc_8bit_overflow() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0xFF;
        cpu.execute(Instr::Inc(R8::A));

        assert_eq!(cpu.regs.a, 0x00);
        check_flags(&cpu, true, false, true, false);
    }

    #[test]
    fn execute_inc_16bit_byte_overflow() {
        let mut cpu = CPU::new();
        cpu.regs.set_bc(0x00FF);
        cpu.execute(Instr::Inc16(R16::BC));

        assert_eq!(cpu.regs.bc(), 0x0100);
        assert_eq!(cpu.regs.b, 0x01);
        assert_eq!(cpu.regs.c, 0x00);
        check_flags(&cpu, false, false, false, false);
    }

    #[test]
    fn execute_inc_16bit_overflow() {
        let mut cpu = CPU::new();
        cpu.regs.set_bc(0xFFFF);
        cpu.execute(Instr::Inc16(R16::BC));

        assert_eq!(cpu.regs.bc(), 0x0);
        assert_eq!(cpu.regs.b, 0x00);
        assert_eq!(cpu.regs.c, 0x00);
        check_flags(&cpu, false, false, false, false);
    }

    // // DEC
    #[test]
    fn execute_dec_8bit_non_overflow() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x7;
        cpu.execute(Instr::Dec(R8::A));

        assert_eq!(cpu.regs.a, 0x6);
        check_flags(&cpu, false, true, false, false);
    }

    #[test]
    fn execute_dec_8bit_half_carry() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x80;
        cpu.execute(Instr::Dec(R8::A));

        assert_eq!(cpu.regs.a, 0x7F);
        check_flags(&cpu, false, true, true, false);
    }

    #[test]
    fn execute_dec_8bit_underflow() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x00;
        cpu.execute(Instr::Dec(R8::A));

        assert_eq!(cpu.regs.a, 0xFF);
        check_flags(&cpu, false, true, true, false);
    }

    #[test]
    fn execute_dec_16bit_underflow() {
        let mut cpu = CPU::new();
        cpu.regs.set_bc(0x0000);
        cpu.execute(Instr::Dec16(R16::BC));

        assert_eq!(cpu.regs.bc(), 0xFFFF);
        assert_eq!(cpu.regs.b, 0xFF);
        assert_eq!(cpu.regs.c, 0xFF);
        check_flags(&cpu, false, false, false, false);
    }

    // ADD
    #[test]
    fn execute_add_8bit_non_overflow_target_a() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x07;
        cpu.execute(Instr::Add(ByteSource::R8(R8::A), false));

        assert_eq!(cpu.regs.a, 0x0E);
        check_flags(&cpu, false, false, false, false);
    }

    #[test]
    fn execute_add_8bit_non_overflow_target_c() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x07;
        cpu.regs.c = 0x03;
        cpu.execute(Instr::Add(ByteSource::R8(R8::C), false));
        assert_eq!(cpu.regs.a, 0x0A);
        check_flags(&cpu, false, false, false, false);
    }

    #[test]
    fn execute_add_8bit_non_overflow_target_c_with_carry() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x07;
        cpu.regs.c = 0x03;
        cpu.regs.set_carry(true);
        cpu.execute(Instr::Add(ByteSource::R8(R8::C), false));
        assert_eq!(cpu.regs.a, 0x0A);
        check_flags(&cpu, false, false, false, false);
    }

    #[test]
    fn execute_add_8bit_carry() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0xFC;
        cpu.regs.b = 0x09;
        cpu.execute(Instr::Add(ByteSource::R8(R8::B), false));
        assert_eq!(cpu.regs.a, 0x05);
        check_flags(&cpu, false, false, true, true);
    }

    // ADDHL
    #[test]
    fn execute_add_hl() {
        let mut cpu = CPU::new();
        cpu.regs.b = 0x07;
        cpu.regs.c = 0x00;
        cpu.regs.h = 0x03;
        cpu.regs.l = 0x00;
        cpu.execute(Instr::AddHL(R16::BC));
        assert_eq!(cpu.regs.hl(), 0x0A00);
        check_flags(&cpu, false, false, true, false);
    }

    // ADC
    #[test]
    fn execute_addc_8bit_non_overflow_target_a_no_carry() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x07;
        cpu.execute(Instr::Add(ByteSource::R8(R8::A), true));
        assert_eq!(cpu.regs.a, 0x0E);
        check_flags(&cpu, false, false, false, false);
    }

    #[test]
    fn execute_addc_8bit_non_overflow_target_a_with_carry() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x07;
        cpu.regs.set_carry(true);
        cpu.execute(Instr::Add(ByteSource::R8(R8::A), true));
        assert_eq!(cpu.regs.a, 0x0F);
        check_flags(&cpu, false, false, false, false);
    }

    #[test]
    fn execute_addc_8bit_non_overflow_target_c_with_carry() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x07;
        cpu.regs.c = 0x03;
        cpu.regs.set_carry(true);

        cpu.execute(Instr::Add(ByteSource::R8(R8::C), true));
        assert_eq!(cpu.regs.a, 0x0b);
        check_flags(&cpu, false, false, false, false);
    }

    #[test]
    fn execute_addc_8bit_carry_with_carry() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0xFC;
        cpu.regs.b = 0x03;
        cpu.regs.set_carry(true);

        cpu.execute(Instr::Add(ByteSource::R8(R8::B), true));
        assert_eq!(cpu.regs.a, 0x00);
        check_flags(&cpu, true, false, true, true);
    }

    // SUB
    #[test]
    fn execute_sub_8bit_non_underflow_target_a() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x07;
        cpu.execute(Instr::Sub(ByteSource::R8(R8::A), false));
        assert_eq!(cpu.regs.a, 0x00);
        check_flags(&cpu, true, true, false, false);
    }

    #[test]
    fn execute_sub_8bit_non_underflow_target_c() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x07;
        cpu.regs.c = 0x03;
        cpu.execute(Instr::Sub(ByteSource::R8(R8::C), false));
        assert_eq!(cpu.regs.a, 0x04);
        check_flags(&cpu, false, true, false, false);
    }

    #[test]
    fn execute_sub_8bit_non_overflow_target_c_with_carry() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x07;
        cpu.regs.c = 0x03;
        cpu.regs.set_carry(true);
        cpu.execute(Instr::Sub(ByteSource::R8(R8::C), false));
        assert_eq!(cpu.regs.a, 0x04);
        check_flags(&cpu, false, true, false, false);
    }

    #[test]
    fn execute_sub_8bit_carry() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x04;
        cpu.regs.b = 0x09;
        cpu.regs.set_carry(true);
        cpu.execute(Instr::Sub(ByteSource::R8(R8::B), false));
        assert_eq!(cpu.regs.a, 0xFB);
        check_flags(&cpu, false, true, true, true);
    }

    // SBC
    #[test]
    fn execute_subc_8bit_non_overflow_target_a_no_carry() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x07;
        cpu.execute(Instr::Sub(ByteSource::R8(R8::A), true));
        assert_eq!(cpu.regs.a, 0x00);
        check_flags(&cpu, true, true, false, false);
    }

    #[test]
    fn execute_subc_8bit_non_overflow_target_a_with_carry() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x07;
        cpu.regs.set_carry(true);
        cpu.execute(Instr::Sub(ByteSource::R8(R8::A), true));
        assert_eq!(cpu.regs.a, 0xFF);
        check_flags(&cpu, false, true, true, true);
    }

    #[test]
    fn execute_subc_8bit_non_overflow_target_c_with_carry() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x07;
        cpu.regs.c = 0x03;
        cpu.regs.set_carry(true);
        cpu.execute(Instr::Sub(ByteSource::R8(R8::C), true));
        assert_eq!(cpu.regs.a, 0x3);
        check_flags(&cpu, false, true, false, false);
    }

    // AND
    #[test]
    fn execute_and_8bit() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x07;
        cpu.execute(Instr::And(ByteSource::R8(R8::A)));
        assert_eq!(cpu.regs.a, 0x7);
        check_flags(&cpu, false, false, true, false);
    }

    #[test]
    fn execute_and_8bit_with_zero() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x08;
        cpu.execute(Instr::And(ByteSource::R8(R8::B)));
        assert_eq!(cpu.regs.a, 0x0);
        check_flags(&cpu, true, false, true, false);
    }

    // OR
    #[test]
    fn execute_or_8bit() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x07;
        cpu.execute(Instr::Or(ByteSource::R8(R8::A)));
        assert_eq!(cpu.regs.a, 0x7);
        check_flags(&cpu, false, false, false, false);
    }

    #[test]
    fn execute_or_8bit_with_zero() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x08;
        cpu.execute(Instr::Or(ByteSource::R8(R8::B)));
        assert_eq!(cpu.regs.a, 0x8);
        check_flags(&cpu, false, false, false, false);
    }

    // XOR
    #[test]
    fn execute_xor_8bit() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0b0000_0111;
        cpu.execute(Instr::Xor(ByteSource::R8(R8::A)));
        assert_eq!(cpu.regs.a, 0x0);
        check_flags(&cpu, true, false, false, false);
    }

    #[test]
    fn execute_xor_8bit_with_zero() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x8;
        cpu.execute(Instr::Xor(ByteSource::R8(R8::B)));
        assert_eq!(cpu.regs.a, 0x08);
        check_flags(&cpu, false, false, false, false);
    }

    // CP
    #[test]
    fn execute_cp_8bit_non_underflow_target_a() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x7;
        cpu.execute(Instr::Cmp(ByteSource::R8(R8::A)));
        assert_eq!(cpu.regs.a, 0x7);
        check_flags(&cpu, true, true, false, false);
    }

    #[test]
    fn execute_cp_8bit_non_underflow_target_c() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x7;
        cpu.regs.c = 0x3;
        cpu.execute(Instr::Cmp(ByteSource::R8(R8::C)));
        assert_eq!(cpu.regs.a, 0x7);
        check_flags(&cpu, false, true, false, false);
    }

    #[test]
    fn execute_cp_8bit_non_overflow_target_c_with_carry() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x7;
        cpu.regs.c = 0x3;
        cpu.regs.set_carry(true);
        cpu.execute(Instr::Cmp(ByteSource::R8(R8::C)));
        assert_eq!(cpu.regs.a, 0x7);
        check_flags(&cpu, false, true, false, false);
    }

    #[test]
    fn execute_cp_8bit_carry() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x4;
        cpu.regs.b = 0x9;
        cpu.execute(Instr::Cmp(ByteSource::R8(R8::B)));
        assert_eq!(cpu.regs.a, 0x4);
        check_flags(&cpu, false, true, true, true);
    }

    // RRA
    #[test]
    fn execute_rra_8bit() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0b1;
        cpu.execute(Instr::RotateRight(R8::A, true));
        assert_eq!(cpu.regs.a, 0x0);
        check_flags(&cpu, false, false, false, true);
    }

    #[test]
    fn execute_rra_8bit_test_carry_used() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x00;
        cpu.regs.set_carry(true);
        cpu.execute(Instr::RotateRight(R8::A, true));
        assert_eq!(cpu.regs.a, 0x80);
        check_flags(&cpu, false, false, false, false);
    }

    // RLA
    #[test]
    fn execute_rla_8bit() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x80;
        cpu.execute(Instr::RotateLeft(R8::A, true));
        assert_eq!(cpu.regs.a, 0x0);
        check_flags(&cpu, false, false, false, true);
    }

    #[test]
    fn execute_rrl_8bit_test_carry_used() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x00;
        cpu.regs.set_carry(true);
        cpu.execute(Instr::RotateLeft(R8::A, true));
        assert_eq!(cpu.regs.a, 0x01);
        check_flags(&cpu, false, false, false, false);
    }

    // RRCA
    #[test]
    fn execute_rrca_8bit() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0b1;
        cpu.regs.set_carry(true);
        cpu.execute(Instr::RotateRight(R8::A, false));
        assert_eq!(cpu.regs.a, 0x80);
        check_flags(&cpu, false, false, false, true);
    }

    #[test]
    fn execute_rrca_8bit_test_carry_not_used() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x00;
        cpu.regs.set_carry(true);
        cpu.execute(Instr::RotateRight(R8::A, false));
        assert_eq!(cpu.regs.a, 0x00);
        check_flags(&cpu, false, false, false, false);
    }

    // RLCA
    #[test]
    fn execute_rlca_8bit() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x80;
        cpu.regs.set_carry(true);
        cpu.execute(Instr::RotateLeft(R8::A, false));
        assert_eq!(cpu.regs.a, 0x1);
        check_flags(&cpu, false, false, false, true);
    }

    #[test]
    fn execute_rlca_8bit_test_carry_not_used() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0x00;
        cpu.regs.set_carry(true);
        cpu.execute(Instr::RotateLeft(R8::A, false));
        assert_eq!(cpu.regs.a, 0x00);
        check_flags(&cpu, false, false, false, false);
    }

    // CPL
    #[test]
    fn execute_cpl_8bit() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0b1011_0100;
        cpu.execute(Instr::ComplementAcc);
        assert_eq!(cpu.regs.a, 0b0100_1011);
        check_flags(&cpu, false, true, true, false);
    }

    // BIT
    #[test]
    fn execute_bit_8bit() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0b1011_0100;
        cpu.execute(Instr::Bit(BitIndex::B2, R8::A));
        assert_eq!(cpu.regs.a, 0b1011_0100);
        check_flags(&cpu, false, false, true, false);

        cpu.execute(Instr::Bit(BitIndex::B1, R8::A));
        assert_eq!(cpu.regs.a, 0b1011_0100);
        check_flags(&cpu, true, false, true, false);
    }

    // RES
    #[test]
    fn execute_res_8bit() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0b1011_0100;
        cpu.execute(Instr::ResetBit(BitIndex::B2, R8::A));
        assert_eq!(cpu.regs.a, 0b1011_0000);
        check_flags(&cpu, false, false, false, false);

        let mut cpu = CPU::new();
        cpu.regs.a = 0b1011_0100;
        cpu.execute(Instr::ResetBit(BitIndex::B1, R8::A));
        assert_eq!(cpu.regs.a, 0b1011_0100);
        check_flags(&cpu, false, false, false, false);
    }

    // SET
    #[test]
    fn execute_set_8bit() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0b1011_0100;
        cpu.execute(Instr::SetBit(BitIndex::B2, R8::A));
        assert_eq!(cpu.regs.a, 0b1011_0100);
        check_flags(&cpu, false, false, false, false);

        let mut cpu = CPU::new();
        cpu.regs.a = 0b1011_0100;
        cpu.execute(Instr::SetBit(BitIndex::B1, R8::A));
        assert_eq!(cpu.regs.a, 0b1011_0110);
        check_flags(&cpu, false, false, false, false);
    }

    // SRL
    #[test]
    fn execute_srl_8bit() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0b1011_0101;
        cpu.execute(Instr::ShiftRightLogical(R8::A));
        assert_eq!(cpu.regs.a, 0b0101_1010);
        check_flags(&cpu, false, false, false, true);
    }

    // RR
    #[test]
    fn execute_rr() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0b1011_0101;
        cpu.execute(Instr::RotateRight(R8::A, true));
        assert_eq!(cpu.regs.a, 0b0101_1010);
        check_flags(&cpu, false, false, false, true);

        let mut cpu = CPU::new();
        cpu.regs.a = 0b1011_0101;
        cpu.regs.set_carry(true);
        cpu.execute(Instr::RotateRight(R8::A, true));
        assert_eq!(cpu.regs.a, 0b1101_1010);
        check_flags(&cpu, false, false, false, true);

        let mut cpu = CPU::new();
        cpu.regs.a = 0b1011_0100;
        cpu.regs.set_carry(true);
        cpu.execute(Instr::RotateRight(R8::A, true));
        assert_eq!(cpu.regs.a, 0b1101_1010);
        check_flags(&cpu, false, false, false, false);
    }

    // RL
    #[test]
    fn execute_rl() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0b1011_0101;
        cpu.execute(Instr::RotateLeft(R8::A, true));
        assert_eq!(cpu.regs.a, 0b0110_1010);
        check_flags(&cpu, false, false, false, true);

        let mut cpu = CPU::new();
        cpu.regs.a = 0b1011_0101;
        cpu.regs.set_carry(true);
        cpu.execute(Instr::RotateLeft(R8::A, true));
        assert_eq!(cpu.regs.a, 0b0110_1011);
        check_flags(&cpu, false, false, false, true);

        let mut cpu = CPU::new();
        cpu.regs.a = 0b0011_0101;
        cpu.regs.set_carry(true);
        cpu.execute(Instr::RotateLeft(R8::A, true));
        assert_eq!(cpu.regs.a, 0b0110_1011);
        check_flags(&cpu, false, false, false, false);
    }

    // SRA
    #[test]
    fn execute_sra() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0b1011_0101;
        cpu.execute(Instr::ShiftRightArithmetic(R8::A));
        assert_eq!(cpu.regs.a, 0b1101_1010);
        check_flags(&cpu, false, false, false, true);
    }

    // SLA
    #[test]
    fn execute_sla() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0b1011_0101;
        cpu.execute(Instr::ShiftLeftArithmetic(R8::A));
        assert_eq!(cpu.regs.a, 0b0110_1010);
        check_flags(&cpu, false, false, false, true);
    }

    // SWAP
    #[test]
    fn execute_swap() {
        let mut cpu = CPU::new();
        cpu.regs.a = 0b1011_0101;
        cpu.execute(Instr::Swap(R8::A));
        assert_eq!(cpu.regs.a, 0b0101_1011);
        check_flags(&cpu, false, false, false, false);
    }

    // JP
    #[test]
    fn execute_jp() {
        let mut cpu = CPU::new();
        cpu.pc = 0xF8;
        cpu.write(0xF9, 0xFC);
        cpu.write(0xFA, 0x02);
        cpu.pc += 1;
        cpu.execute(Instr::JumpAbsolute(Condition::Always, JumpSource::N16));
        assert_eq!(cpu.pc, 0x02FC);

        let mut cpu = CPU::new();
        cpu.pc = 0xF8;
        cpu.write(0xF9, 0xFC);
        cpu.write(0xFA, 0x02);
        cpu.pc += 1; // pretend we step
        cpu.execute(Instr::JumpAbsolute(Condition::Carry, JumpSource::N16));
        assert_eq!(cpu.pc, 0xFB);
    }

    // JR
    #[test]
    fn execute_jr() {
        let mut cpu = CPU::new();
        cpu.pc = 0xF8;
        cpu.write(0xF9, 0x4);
        cpu.pc += 1; // pretend we step
        cpu.execute(Instr::JumpRelative(Condition::Always));
        assert_eq!(cpu.pc, 0xFE);

        let mut cpu = CPU::new();
        cpu.pc = 0xF8;
        cpu.write(0xF9, 0xFC);
        cpu.pc += 1; // pretend we step
        cpu.execute(Instr::JumpRelative(Condition::Always));
        assert_eq!(cpu.pc, 0xF6);
    }

    // LD a, (??)
    #[test]
    fn execute_ld_a_indirect() {
        let mut cpu = CPU::new();
        cpu.regs.set_bc(0xF9);
        cpu.write(0xF9, 0x4);
        cpu.execute(Instr::LoadIndirectToAcc(R16Mem::BC));

        assert_eq!(cpu.regs.a, 0x04);

        cpu.regs.set_hl(0xA1);
        cpu.write(0xA1, 0x9);
        cpu.execute(Instr::LoadIndirectToAcc(R16Mem::HLInc));

        assert_eq!(cpu.regs.a, 0x09);
        assert_eq!(cpu.regs.hl(), 0xA2);
    }

    // LD ?, ?
    #[test]
    fn execute_ld_byte() {
        let mut cpu = CPU::new();
        cpu.regs.b = 0x4;
        cpu.execute(Instr::Load(R8::D, ByteSource::R8(R8::B)));

        assert_eq!(cpu.regs.b, 0x4);
        assert_eq!(cpu.regs.d, 0x4);
    }

    // PUSH/POP
    #[test]
    fn execute_push_pop() {
        let mut cpu = CPU::new();
        cpu.regs.b = 0x04;
        cpu.regs.c = 0x89;
        cpu.sp = 0x10;

        cpu.execute(Instr::Push(R16Stack::BC));

        assert_eq!(cpu.read(0xF), 0x04);
        assert_eq!(cpu.read(0xE), 0x89);
        assert_eq!(cpu.sp, 0xE);

        cpu.execute(Instr::Pop(R16Stack::DE));

        assert_eq!(cpu.regs.d, 0x04);
        assert_eq!(cpu.regs.e, 0x89);
    }

    // -----------------------------------------------------------------------------

    // Step
    #[test]
    fn test_step() {
        let mut cpu = CPU::new();
        cpu.write(0, 0x23); //INC(HL)
        cpu.write(1, 0xB5); //OR(L)
        cpu.write(2, 0xCB); //PREFIX
        cpu.write(3, 0xe8); //SET(B, 5)

        cpu.step();
        cpu.step();
        cpu.step();

        assert_eq!(cpu.regs.h, 0b0);
        assert_eq!(cpu.regs.l, 0b1);
        assert_eq!(cpu.regs.a, 0b1);
        assert_eq!(cpu.regs.b, 0b0010_0000);
    }
}
