use crate::graphics::Graphics;

pub(crate) const VRAM_BEGIN: usize = 0x8000;
pub(crate) const VRAM_END: usize = 0x9FFF;
pub(crate) const VRAM_SIZE: usize = VRAM_END - VRAM_BEGIN + 1;
#[derive(Debug, Clone)]
pub(crate) struct Memory {
    memory: [u8; 65535],
    gpu: Graphics,
}

impl Memory {
    pub(crate) fn new() -> Self {
        Self {
            memory: [0; 65535],
            gpu: Graphics::new(),
        }
    }

    pub(crate) fn read(&self, address: u16) -> u8 {
        let address = usize::from(address);
        match address {
            VRAM_BEGIN..VRAM_END => self.gpu.read_vram(address - VRAM_BEGIN),
            _ => self.memory[address],
        }
    }

    pub(crate) fn write(&mut self, address: u16, value: u8) {
        let address = usize::from(address);
        match address {
            VRAM_BEGIN..VRAM_END => self.gpu.write_vram(address - VRAM_BEGIN, value),
            _ => self.memory[address] = value,
        }
    }
}
