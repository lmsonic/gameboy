use crate::{
    instruction2::{ByteSource, Instr, LoadHalfTarget, R16Mem, R16, R8},
    registers::Registers,
};

#[derive(Debug, Clone)]
#[allow(clippy::upper_case_acronyms)]
struct CPU {
    regs: Registers,
    pc: u16,
    sp: u16,
    memory: [u8; 65535],
}

impl CPU {
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
        self.pc += 1;
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
                self.regs.a += 1;

                self.regs.set_zero(self.regs.a == 0);
                self.regs.set_neg(false);
                self.regs.set_half_carry((self.regs.a & 0xF) + 1 > 0xF);
            }
            Instr::Dec(r8) => {
                self.regs.a -= 1;

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
            Instr::RotateLeft(r8) => todo!(),
            Instr::RotateRight(r8) => todo!(),
            Instr::RotateLeftThruCarry(r8) => todo!(),
            Instr::RotateRightThruCarry(r8) => todo!(),
            Instr::ShiftLeftArithmetic(r8) => todo!(),
            Instr::ShiftRightArithmetic(r8) => todo!(),
            Instr::Swap(r8) => todo!(),
            Instr::ShiftRightLogical(r8) => todo!(),
            Instr::Bit(bit_index, r8) => todo!(),
            Instr::ResetBit(bit_index, r8) => todo!(),
            Instr::SetBit(bit_index, r8) => todo!(),
            Instr::DecimalAdjustAcc => todo!(),
            Instr::ComplementAcc => todo!(),
            Instr::SetCarryFlag => todo!(),
            Instr::ClearCarryFlag => todo!(),
            Instr::JumpRelative(condition) => todo!(),
            Instr::JumpAbsolute(condition, jump_source) => todo!(),
            Instr::Stop => todo!(),
            Instr::Halt => todo!(),
            Instr::Ret(condition) => todo!(),
            Instr::RetInterrupt => todo!(),
            Instr::Call(condition) => todo!(),
            Instr::CallRST(rstaddress) => todo!(),
            Instr::Pop(r16_stack) => todo!(),
            Instr::Push(r16_stack) => todo!(),
            Instr::AddSP => todo!(),
            Instr::LoadSPPlusImmediateToHL => todo!(),
            Instr::LoadHLToSP => todo!(),
            Instr::DisableInterrupt => todo!(),
            Instr::EnableInterrupt => todo!(),
        }
    }
}
