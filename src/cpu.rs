use crate::registers::Registers;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Instruction {
    Add(AritmeticTarget),
    AddHL(AritmeticTarget),
    AddCarry(AritmeticTarget),
    Sub(AritmeticTarget),
    SubCarry(AritmeticTarget),
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AritmeticTarget {
    A,
    B,
    C,
    D,
    E,
    F,
    H,
    L,
}
#[derive(Debug, Clone, Copy)]
#[allow(clippy::upper_case_acronyms)]
struct CPU {
    regs: Registers,
}
impl CPU {
    fn execute(&mut self, instruction: Instruction) {
        match instruction {
            Instruction::Add(aritmetic_target) => {
                self.regs.a = self.add(self.register_from_target(aritmetic_target))
            }
            Instruction::AddHL(aritmetic_target) => {
                let new_value = self.addhl(self.register_from_target(aritmetic_target));
                self.regs.set_hl(new_value)
            }
            Instruction::AddCarry(aritmetic_target) => {
                self.regs.a = self.add(self.register_from_target(aritmetic_target))
                    + u8::from(self.regs.f.carry)
            }
            Instruction::Sub(aritmetic_target) => {
                self.regs.a = self.sub(self.register_from_target(aritmetic_target))
            }
            Instruction::SubCarry(aritmetic_target) => {
                self.regs.a = self.sub(self.register_from_target(aritmetic_target))
                    - u8::from(self.regs.f.carry)
            }
        }
    }
    fn register_from_target(&self, target: AritmeticTarget) -> u8 {
        match target {
            AritmeticTarget::A => self.regs.a,
            AritmeticTarget::B => self.regs.b,
            AritmeticTarget::C => self.regs.c,
            AritmeticTarget::D => self.regs.d,
            AritmeticTarget::E => self.regs.e,
            AritmeticTarget::F => u8::from(self.regs.f),
            AritmeticTarget::H => self.regs.h,
            AritmeticTarget::L => self.regs.l,
        }
    }
    fn add(&mut self, value: u8) -> u8 {
        let (new_value, overflow) = self.regs.a.overflowing_add(value);
        // TODO: set flags
        self.regs.f.zero = new_value == 0;
        self.regs.f.subtract = false;
        self.regs.f.carry = overflow;
        // Half Carry is set if adding the lower nibbles of the value and register A
        // together result in a value bigger than 0xF. If the result is larger than 0xF
        // than the addition caused a carry from the lower nibble to the upper nibble.
        self.regs.f.half_carry = (self.regs.a & 0xF) + (value & 0xF) > 0xF;
        new_value
    }
    fn sub(&mut self, value: u8) -> u8 {
        let (new_value, overflow) = self.regs.a.overflowing_sub(value);
        // TODO: set flags
        self.regs.f.zero = new_value == 0;
        self.regs.f.subtract = true;
        self.regs.f.carry = overflow;
        // Half Carry is set if adding the lower nibbles of the value and register A
        // together result in a value bigger than 0xF. If the result is larger than 0xF
        // than the addition caused a carry from the lower nibble to the upper nibble.
        self.regs.f.half_carry = (self.regs.a & 0xF) + (value & 0xF) > 0xF;
        new_value
    }
    fn addhl(&mut self, value: u8) -> u16 {
        let (new_value, overflow) = self.regs.hl().overflowing_add(u16::from(value));
        // TODO: set flags
        self.regs.f.zero = new_value == 0;
        self.regs.f.subtract = false;
        self.regs.f.carry = overflow;
        // Half Carry is set if adding the lower nibbles of the value and register A
        // together result in a value bigger than 0xF. If the result is larger than 0xF
        // than the addition caused a carry from the lower nibble to the upper nibble.
        self.regs.f.half_carry = (self.regs.a & 0xF) + (value & 0xF) > 0xF;
        new_value
    }
}
