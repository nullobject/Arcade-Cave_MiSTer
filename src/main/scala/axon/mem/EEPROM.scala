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

package axon.mem

import axon.Util
import chisel3._
import chisel3.util._

/** An interface for communicating with a serial I/O device. */
class SerialIO extends Bundle {
  /** Chip select */
  val cs = Input(Bool())
  /** Serial clock */
  val sck = Input(Bool())
  /** Serial data in */
  val sdi = Input(Bool())
  /** Serial data out */
  val sdo = Output(Bool())
}

/** Presents an asynchronous memory interface as a serial EEPROM. */
class EEPROM extends Module {
  val io = IO(new Bundle {
    /** Memory port */
    val mem = AsyncReadWriteMemIO(EEPROM.ADDR_WIDTH, EEPROM.DATA_WIDTH)
    /** Serial port */
    val serial = new SerialIO
    /** Debug port */
    val debug = Output(new Bundle {
      val idle = Bool()
      val command = Bool()
      val read = Bool()
      val write = Bool()
      val erase = Bool()
      val shift = Bool()
      val readWait = Bool()
      val writeWait = Bool()
    })
  })

  // States
  object State {
    val idle :: command :: read :: write :: erase :: shift :: readWait :: writeWait :: Nil = Enum(8)
  }

  object Command {
    val read :: write :: writeAll :: erase :: eraseAll :: Nil = Enum(5)
  }

  // State register
  val nextState = Wire(UInt())
  val stateReg = RegNext(nextState, State.idle)

  // Command register
  val nextCommand = Wire(UInt())
  val commandReg = RegNext(nextCommand, Command.read)

  // Serial clock
  val serialClock = io.serial.cs && Util.rising(io.serial.sck)

  // Counters
  val (bitCounter, _) = Counter(0 until EEPROM.DATA_WIDTH,
    enable = (stateReg === State.command || stateReg === State.shift) && serialClock,
    reset = nextState =/= stateReg
  )

  // Registers
  val dataReg = Reg(UInt(EEPROM.DATA_WIDTH.W))
  val opcodeReg = Reg(UInt(EEPROM.OPCODE_WIDTH.W))
  val addrReg = Reg(UInt(EEPROM.ADDR_WIDTH.W))
  val writeEnableReg = RegInit(false.B)

  // Control signals
  val opcodeLatch = bitCounter <= (EEPROM.OPCODE_WIDTH - 1).U
  val commandDone = bitCounter === (EEPROM.OPCODE_WIDTH + EEPROM.ADDR_WIDTH - 1).U
  val bitsDone = bitCounter === (EEPROM.DATA_WIDTH - 1).U
  val read = opcodeReg === 2.U
  val write = opcodeReg === 1.U && writeEnableReg
  val writeAll = opcodeReg === 0.U && addrReg(4, 3) === 1.U && writeEnableReg
  val erase = opcodeReg === 3.U && writeEnableReg
  val eraseAll = opcodeReg === 0.U && addrReg(4, 3) === 2.U && writeEnableReg
  val enableWrite = opcodeReg === 0.U && addrReg(4, 3) === 3.U
  val disableWrite = opcodeReg === 0.U && addrReg(4, 3) === 0.U

  // Update serial data register
  val serialReg = RegNext(MuxLookup(stateReg, false.B, Seq(
    State.idle -> true.B,
    State.shift -> dataReg.head(1)
  )), false.B)

  // Toggle write enable
  when(stateReg === State.command && serialClock && commandDone) {
    when(enableWrite) {
      writeEnableReg := true.B
    }.elsewhen(disableWrite) {
      writeEnableReg := false.B
    }
  }

  // Update opcode register
  when(stateReg === State.command && serialClock && opcodeLatch) {
    opcodeReg := opcodeReg ## io.serial.sdi
  }

  // Update address register
  when(stateReg === State.command && serialClock) {
    addrReg := addrReg ## io.serial.sdi
    when(commandDone && opcodeReg === 0.U) { addrReg := 0.U }
  }.elsewhen(stateReg === State.writeWait && !io.mem.waitReq) {
    addrReg := addrReg + 1.U
  }

  // Update data register
  when(stateReg === State.idle) {
    dataReg := 0xffff.U
  }.elsewhen(stateReg === State.readWait && io.mem.valid) {
    dataReg := io.mem.dout
  }.elsewhen(stateReg === State.shift && serialClock) {
    dataReg := dataReg ## io.serial.sdi
  }

  // Default to the previous state
  nextState := stateReg

  // Default to the previous command
  nextCommand := commandReg

  // FSM
  switch(stateReg) {
    // Wait for start bit
    is(State.idle) {
      when(serialClock && io.serial.sdi) { nextState := State.command }
    }

    // Wait for opcode/address
    is(State.command) {
      when(serialClock && commandDone) {
        when(read) {
          nextState := State.read
          nextCommand := Command.read
        }.elsewhen(write) {
          nextState := State.write
          nextCommand := Command.write
        }.elsewhen(writeAll) {
          nextState := State.write
          nextCommand := Command.writeAll
        }.elsewhen(erase) {
          nextState := State.erase
          nextCommand := Command.erase
        }.elsewhen(eraseAll) {
          nextState := State.erase
          nextCommand := Command.eraseAll
        }.otherwise {
          nextState := State.idle
        }
      }
    }

    // Begin a read command
    is(State.read) {
      when(!io.mem.waitReq) { nextState := State.readWait }
    }

    // Begin a write command
    is(State.write) {
      nextState := State.shift
    }

    // Begin a erase command
    is(State.erase) {
      nextState := State.writeWait
    }

    // Shift serial data
    is(State.shift) {
      when(serialClock && bitsDone) {
        nextState := Mux(commandReg === Command.read, State.idle, State.writeWait)
      }
    }

    // Wait for memory read
    is(State.readWait) {
      when(io.mem.valid) { nextState := State.shift }
    }

    // Wait for memory write
    is(State.writeWait) {
      when(!io.mem.waitReq) {
        when(commandReg === Command.writeAll && addrReg < (EEPROM.DEPTH - 1).U) {
          nextState := State.shift
        }.elsewhen(commandReg === Command.eraseAll && addrReg < (EEPROM.DEPTH - 1).U) {
          nextState := State.writeWait
        }.otherwise {
          nextState := State.idle
        }
      }
    }
  }

  // Return to idle state when chip select is deasserted
  when(!io.serial.cs) { nextState := State.idle }

  // Outputs
  io.serial.sdo := serialReg
  io.mem.rd := stateReg === State.read
  io.mem.wr := stateReg === State.writeWait
  io.mem.addr := addrReg
  io.mem.mask := 2.U
  io.mem.din := dataReg
  io.debug.idle := stateReg === State.idle
  io.debug.command := stateReg === State.command
  io.debug.read := stateReg === State.read
  io.debug.write := stateReg === State.write
  io.debug.erase := stateReg === State.erase
  io.debug.shift := stateReg === State.shift
  io.debug.readWait := stateReg === State.readWait
  io.debug.writeWait := stateReg === State.writeWait

  printf(p"EEPROM(state: $stateReg, command: $commandReg, bitCounter: $bitCounter, data: ${ Binary(dataReg) }, opcode: ${ Binary(opcodeReg) }, addr: 0x${ Hexadecimal(addrReg) })\n")
}

object EEPROM {
  /** The width of the EEPROM opcode */
  val OPCODE_WIDTH = 2
  /** The width of the EEPROM address bus */
  val ADDR_WIDTH = 6
  /** The width of the EEPROM data bus */
  val DATA_WIDTH = 16
  /** The number of words in the EEPROM */
  val DEPTH = 64
}
