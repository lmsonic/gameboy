use crate::{
    instruction2::{
        ByteSource, Condition, Instr, JumpSource, LoadHalfTarget, R16Mem, R16Stack, RSTAddress,
        R16, R8,
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
        let byte = self.read(self.pc);
        self.pc += 1;
        byte
    }
    fn fetch_word(&mut self) -> u16 {
        let lsb = u16::from(self.fetch());
        let msb = u16::from(self.fetch());
        (msb << 8) | lsb
    }
    fn step(&mut self) {
        let byte = self.fetch();
        let instruction = if byte == 0xCB {
            let byte = self.fetch();
            Instr::from_0xCB_prefixed_opcode(byte)
        } else {
            Instr::from_opcode(byte)
        };
        self.execute(instruction);
        if !self.is_halted {
            self.pc += 1;
        }
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
                let value = self.get_byte_source(byte_source)
                    + if with_carry {
                        u8::from(self.regs.carry())
                    } else {
                        0
                    };
                let (new_value, overflow) = self.regs.a.overflowing_add(value);
                self.regs.a = new_value;

                self.regs.set_zero(new_value == 0);
                self.regs.set_neg(false);
                self.regs
                    .set_half_carry((self.regs.a & 0xF) + (value & 0xF) > 0xF);
                self.regs.set_carry(overflow);
            }

            Instr::Sub(byte_source, with_carry) => {
                let value = self.get_byte_source(byte_source)
                    - if with_carry {
                        u8::from(self.regs.carry())
                    } else {
                        0
                    };
                let (new_value, overflow) = self.regs.a.overflowing_sub(value);
                self.regs.a = new_value;

                self.regs.set_zero(new_value == 0);
                self.regs.set_neg(true);
                self.regs
                    .set_half_carry((self.regs.a & 0xF) < (value & 0xF));
                self.regs.set_carry(overflow);
            }
            Instr::And(byte_source) => {
                let value = self.get_byte_source(byte_source);
                self.regs.a &= value;

                self.regs.set_zero(self.regs.a == 0);
                self.regs.set_neg(true);
                self.regs.set_half_carry(false);
                self.regs.set_carry(true);
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
                self.set_reg(r8, self.reg(r8) + 1);

                self.regs.set_zero(self.regs.a == 0);
                self.regs.set_neg(false);
                self.regs.set_half_carry((self.regs.a & 0xF) + 1 > 0xF);
            }
            Instr::Dec(r8) => {
                self.set_reg(r8, self.reg(r8) - 1);

                self.regs.set_zero(self.regs.a == 0);
                self.regs.set_neg(true);
                self.regs.set_half_carry((self.regs.a & 0xF) == 0);
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
                self.set_reg16(r16, value + 1);
            }
            Instr::Dec16(r16) => {
                let value = self.reg16(r16);
                self.set_reg16(r16, value - 1);
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
                self.regs.set_neg(false);
                self.regs.set_half_carry(false);
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
                    if offset > 0 {
                        self.pc += offset as u16;
                    } else {
                        self.pc -= offset as u16;
                    }
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
        self.sp -= 1;
        let lsb = (value & 0xFF00 >> 8) as u8;
        self.write(self.sp, lsb);
        self.sp -= 1;
        let msb = (value & 0x00FF) as u8;
        self.write(self.sp, msb);
    }
    fn pop(&mut self) -> u16 {
        let lsb = u16::from(self.read(self.sp));
        self.sp += 1;
        let msb = u16::from(self.read(self.sp));
        self.sp += 1;
        msb << 8 | lsb
    }
}
