/*
 *   __   __     __  __     __         __
 *  /\ "-.\ \   /\ \/\ \   /\ \       /\ \
 *  \ \ \-.  \  \ \ \_\ \  \ \ \____  \ \ \____
 *   \ \_\\"\_\  \ \_____\  \ \_____\  \ \_____\
 *    \/_/ \/_/   \/_____/   \/_____/   \/_____/
 *   ______     ______       __     ______     ______     ______
 *  /\  __ \   /\  == \     /\ \   /\  ___\   /\  ___\   /\__  _\
 *  \ \ \/\ \  \ \  __<    _\_\ \  \ \  __\   \ \ \____  \/_/\ \/
 *   \ \_____\  \ \_____\ /\_____\  \ \_____\  \ \_____\    \ \_\
 *    \/_____/   \/_____/ \/_____/   \/_____/   \/_____/     \/_/
 *
 * https://joshbassett.info
 * https://twitter.com/nullobject
 * https://github.com/nullobject
 *
 * Copyright (c) 2021 Josh Bassett
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

package cave

import axon._
import axon.cpu.m68k._
import axon.mem._
import axon.snd._
import axon.types._
import cave.gpu._
import cave.types._
import chisel3._
import chisel3.util._

/**
 * Represents the first-generation CAVE arcade hardware.
 *
 * This module contains the CPU, GPU, sound processor, RAM, ROM, and memory maps.
 */
class Cave extends Module {
  /**
   * Encodes the player IO into a bitvector value.
   *
   * @param player The player interface.
   * @return A bitvector representing the player inputs.
   */
  private def encodePlayer(player: PlayerIO): Bits = {
    // If the coin signal is asserted for too long (i.e. the player holds the coin button down),
    // then it will trigger a "coin error" and the game will reboot. To prevent this from happening,
    // the coin signal must be converted to a pulse.
    val coin = Util.pulseSync(Config.PLAYER_COIN_PULSE_WIDTH, player.coin)
    Cat(coin, player.start, player.buttons, player.right, player.left, player.down, player.up)
  }

  val io = IO(new Bundle {
    /** CPU clock domain */
    val cpuClock = Input(Clock())
    /** CPU reset */
    val cpuReset = Input(Reset())
    /** Vertical blank */
    val vBlank = Input(Bool())
    /** Asserted when a frame is ready */
    val frameReady = Output(Bool())
    /** Asserted when the DMA controller is ready */
    val dmaReady = Input(Bool())
    /** Game config port */
    val gameConfig = Input(GameConfig())
    /** Options port */
    val options = OptionsIO()
    /** Joystick port */
    val joystick = JoystickIO()
    /** Program ROM port */
    val progRom = new ProgRomIO
    /** Sound ROM port */
    val soundRom = new SoundRomIO
    /** EEPROM port */
    val eeprom = new EEPROMIO
    /** Tile ROM port */
    val tileRom = new TileRomIO
    /** Audio port */
    val audio = Output(new Audio(Config.ymzConfig.sampleWidth))
    /** Frame buffer DMA port */
    val frameBufferDMA = Flipped(new FrameBufferDMAIO)
  })

  // Wires
  val frameStart = WireInit(false.B)
  val intAck = Wire(Bool())

  // GPU
  val gpu = Module(new GPU)
  gpu.io.frameReq := Util.rising(ShiftRegister(frameStart, 2))
  io.frameReady := gpu.io.frameReady
  gpu.io.dmaReady := io.dmaReady
  gpu.io.gameConfig <> io.gameConfig
  gpu.io.options <> io.options
  io.tileRom <> gpu.io.tileRom
  io.frameBufferDMA <> gpu.io.frameBufferDMA

  // The CPU and registers run in the CPU clock domain
  withClockAndReset(io.cpuClock, io.cpuReset) {
    // Registers
    val vBlank = ShiftRegister(io.vBlank, 2)
    val vBlankIRQ = RegInit(false.B)
    val iplReg = RegInit(0.U)
    val pauseReg = Util.toggle(Util.rising(io.joystick.player1.pause || io.joystick.player2.pause))

    // M68K CPU
    val cpu = Module(new CPU)
    cpu.io.halt := pauseReg
    cpu.io.dtack := false.B
    cpu.io.vpa := intAck // autovectored interrupts
    cpu.io.ipl := iplReg
    cpu.io.din := 0.U

    // EEPROM
    val eeprom = Module(new EEPROM)

    // Main RAM
    val mainRam = Module(new SinglePortRam(
      addrWidth = Config.MAIN_RAM_ADDR_WIDTH,
      dataWidth = Config.MAIN_RAM_DATA_WIDTH
    ))

    // Sprite RAM
    val spriteRam = Module(new TrueDualPortRam(
      addrWidthA = Config.SPRITE_RAM_ADDR_WIDTH,
      dataWidthA = Config.SPRITE_RAM_DATA_WIDTH,
      addrWidthB = Config.SPRITE_RAM_GPU_ADDR_WIDTH,
      dataWidthB = Config.SPRITE_RAM_GPU_DATA_WIDTH
    ))
    spriteRam.io.clockB := clock

    // Layer 0 RAM
    val layer0Ram = Module(new TrueDualPortRam(
      addrWidthA = Config.LAYER_RAM_ADDR_WIDTH,
      dataWidthA = Config.LAYER_RAM_DATA_WIDTH,
      addrWidthB = Config.LAYER_RAM_GPU_ADDR_WIDTH,
      dataWidthB = Config.LAYER_RAM_GPU_DATA_WIDTH
    ))
    layer0Ram.io.clockB := clock

    // Layer 1 RAM
    val layer1Ram = Module(new TrueDualPortRam(
      addrWidthA = Config.LAYER_RAM_ADDR_WIDTH,
      dataWidthA = Config.LAYER_RAM_DATA_WIDTH,
      addrWidthB = Config.LAYER_RAM_GPU_ADDR_WIDTH,
      dataWidthB = Config.LAYER_RAM_GPU_DATA_WIDTH
    ))
    layer1Ram.io.clockB := clock

    // Layer 2 RAM
    val layer2Ram = Module(new TrueDualPortRam(
      addrWidthA = Config.LAYER_RAM_ADDR_WIDTH,
      dataWidthA = Config.LAYER_RAM_DATA_WIDTH,
      addrWidthB = Config.LAYER_RAM_GPU_ADDR_WIDTH,
      dataWidthB = Config.LAYER_RAM_GPU_DATA_WIDTH
    ))
    layer2Ram.io.clockB := clock

    // Palette RAM
    val paletteRam = Module(new TrueDualPortRam(
      addrWidthA = Config.PALETTE_RAM_ADDR_WIDTH,
      dataWidthA = Config.PALETTE_RAM_DATA_WIDTH,
      addrWidthB = Config.PALETTE_RAM_GPU_ADDR_WIDTH,
      dataWidthB = Config.PALETTE_RAM_GPU_DATA_WIDTH
    ))
    paletteRam.io.clockB := clock

    // Layer registers
    val layer0Regs = Module(new RegisterFile(Config.LAYER_REGS_COUNT))
    val layer1Regs = Module(new RegisterFile(Config.LAYER_REGS_COUNT))
    val layer2Regs = Module(new RegisterFile(Config.LAYER_REGS_COUNT))

    // Video registers
    val videoRegs = Module(new RegisterFile(Config.VIDEO_REGS_COUNT))

    // GPU
    gpu.io.videoRegs := videoRegs.io.regs.asUInt
    gpu.io.layer0Regs := layer0Regs.io.regs.asUInt
    gpu.io.layer1Regs := layer1Regs.io.regs.asUInt
    gpu.io.layer2Regs := layer2Regs.io.regs.asUInt
    gpu.io.spriteRam <> spriteRam.io.portB
    gpu.io.layer0Ram <> layer0Ram.io.portB
    gpu.io.layer1Ram <> layer1Ram.io.portB
    gpu.io.layer2Ram <> layer2Ram.io.portB
    gpu.io.paletteRam <> paletteRam.io.portB

    // YMZ280B
    val ymz = Module(new YMZ280B(Config.ymzConfig))
    ymz.io.mem <> io.soundRom
    io.audio <> RegEnable(ymz.io.audio.bits, ymz.io.audio.valid)
    val soundIRQ = ymz.io.irq

    // TODO
    io.eeprom.default()

    // Interrupt acknowledge
    intAck := cpu.io.as && cpu.io.fc === 7.U

    // Set and clear interrupt priority level register
    when(vBlankIRQ || soundIRQ) { iplReg := 1.U }.elsewhen(intAck) { iplReg := 0.U }

    // Set vertical blank IRQ
    when(Util.rising(vBlank)) { vBlankIRQ := true.B }

    // Set memory interface defaults, the actual values are assigned in the memory map
    videoRegs.io.mem.default()
    layer0Regs.io.mem.default()
    layer1Regs.io.mem.default()
    layer2Regs.io.mem.default()
    layer0Ram.io.portA.default()
    layer1Ram.io.portA.default()
    layer2Ram.io.portA.default()
    paletteRam.io.portA.default()
    eeprom.io.mem.default()

    // Set input ports
    val input0 = "b111111".U ## ~io.joystick.service1 ## ~encodePlayer(io.joystick.player1)
    val input1 = "b1111".U ## eeprom.io.dout ## "b11".U ## ~encodePlayer(io.joystick.player2)

    // Memory map
    val map = new MemMap(cpu.io)
    map(0x000000 to 0x0fffff).readMem(io.progRom)
    map(0x100000 to 0x10ffff).readWriteMem(mainRam.io)
    // Access to 0x11xxxx appears during the service menu. It must be ignored, otherwise the service
    // menu freezes.
    map(0x110000 to 0x2fffff).ignore()
    map(0x300000 to 0x300003).readWriteMem(ymz.io.cpu)
    map(0x400000 to 0x40ffff).readWriteMem(spriteRam.io.portA)

    // Dangun Feveron
    when(io.gameConfig.index === GameConfig.DFEVERON.U) {
      map(0x500000 to 0x507fff).readWriteMem(layer0Ram.io.portA)
      map(0x600000 to 0x607fff).readWriteMem(layer1Ram.io.portA)
      map(0x708000 to 0x708fff).readWriteMemT(paletteRam.io.portA)(a => a(10, 0))
      map(0x710000 to 0x717fff).readWriteMem(layer2Ram.io.portA)
      map(0x800000 to 0x80007f).writeMem(videoRegs.io.mem.asWriteMemIO)
      map(0x800004).w { (_, _, data) => frameStart := data === 0x01f0.U }
      map(0x800000 to 0x800007).r { (_, offset) =>
        when(offset === 0.U) { vBlankIRQ := false.B } // clear vertical blank IRQ
        Cat(0.U, 1.U, !vBlankIRQ)
      }
      map(0x900000 to 0x900005).readWriteMem(layer0Regs.io.mem)
      map(0xa00000 to 0xa00005).readWriteMem(layer1Regs.io.mem)
      map(0xb00000).r { (_, _) => input0 }
      map(0xb00002).r { (_, _) => input1 }
      map(0xc00000).writeMem(eeprom.io.mem)
    }

    // DoDonPachi
    when(io.gameConfig.index === GameConfig.DDONPACH.U) {
      map(0x500000 to 0x507fff).readWriteMem(layer0Ram.io.portA)
      map(0x600000 to 0x607fff).readWriteMem(layer1Ram.io.portA)
      // Access to 0x5fxxxx appears in DoDonPachi on attract loop when showing the air stage on
      // frame 9355 (i.e. after roughly 2 min 30 sec). The game is accessing data relative to a
      // Layer 1 address and underflows. These accesses do nothing, but should be acknowledged in
      // order not to block the CPU.
      //
      // The reason these accesses appear is probably because it made the layer update routine
      // simpler to write (no need to handle edge cases). These accesses are simply ignored by the
      // hardware.
      map(0x5f0000 to 0x5fffff).ignore()
      map(0x700000 to 0x70ffff).readWriteMemT(layer2Ram.io.portA)(a => a(12, 0))
      map(0x800000 to 0x80007f).writeMem(videoRegs.io.mem.asWriteMemIO)
      map(0x800004).w { (_, _, data) => frameStart := data === 0x01f0.U }
      map(0x800000 to 0x800007).r { (_, offset) =>
        when(offset === 0.U) { vBlankIRQ := false.B } // clear vertical blank IRQ
        Cat(1.U, 1.U, !vBlankIRQ)
      }
      map(0x900000 to 0x900005).readWriteMem(layer0Regs.io.mem)
      map(0xa00000 to 0xa00005).readWriteMem(layer1Regs.io.mem)
      map(0xb00000 to 0xb00005).readWriteMem(layer2Regs.io.mem)
      map(0xc00000 to 0xc0ffff).readWriteMem(paletteRam.io.portA)
      map(0xd00000).r { (_, _) => input0 }
      map(0xd00002).r { (_, _) => input1 }
      map(0xe00000).writeMem(eeprom.io.mem)
    }

    // ESP Ra.De.
    when(io.gameConfig.index === GameConfig.ESPRADE.U) {
      map(0x500000 to 0x507fff).readWriteMem(layer0Ram.io.portA)
      map(0x600000 to 0x607fff).readWriteMem(layer1Ram.io.portA)
      map(0x700000 to 0x707fff).readWriteMem(layer2Ram.io.portA)
      map(0x800000 to 0x80007f).writeMem(videoRegs.io.mem.asWriteMemIO)
      map(0x800004).w { (_, _, data) => frameStart := data === 0x01f0.U }
      map(0x800000 to 0x800007).r { (_, offset) =>
        when(offset === 0.U) { vBlankIRQ := false.B } // clear vertical blank IRQ
        Cat(0.U, 1.U, !vBlankIRQ)
      }
      map(0x800008 to 0x800fff).ignore()
      map(0x900000 to 0x900005).readWriteMem(layer0Regs.io.mem)
      map(0xa00000 to 0xa00005).readWriteMem(layer1Regs.io.mem)
      map(0xb00000 to 0xb00005).readWriteMem(layer2Regs.io.mem)
      map(0xc00000 to 0xc0ffff).readWriteMem(paletteRam.io.portA)
      map(0xd00000).r { (_, _) => input0 }
      map(0xd00002).r { (_, _) => input1 }
      map(0xe00000).writeMem(eeprom.io.mem)
    }

    // Puzzle Uo Poko
    when(io.gameConfig.index === GameConfig.UOPOKO.U) {
      map(0x500000 to 0x507fff).readWriteMem(layer0Ram.io.portA)
      map(0x600000 to 0x60007f).writeMem(videoRegs.io.mem.asWriteMemIO)
      map(0x600004).w { (_, _, data) => frameStart := data === 0x01f0.U }
      map(0x600000 to 0x600007).r { (_, offset) =>
        when(offset === 0.U) { vBlankIRQ := false.B } // clear vertical blank IRQ
        Cat(0.U, 1.U, !vBlankIRQ)
      }
      map(0x600008 to 0x600fff).ignore()
      map(0x700000 to 0x700005).readWriteMem(layer0Regs.io.mem)
      map(0x800000 to 0x80ffff).readWriteMem(paletteRam.io.portA)
      map(0x900000).r { (_, _) => input0 }
      map(0x900002).r { (_, _) => input1 }
      map(0xa00000).writeMem(eeprom.io.mem)
    }

    // When the game is paused, request frames at the start of every vertical blank
    when(pauseReg) { frameStart := vBlank }
  }
}
