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

import chisel3._
import chiseltest._
import org.scalatest._

trait EEPROMTestHelpers {
  protected def startBit(dut: EEPROM) = {
    dut.io.serial.cs.poke(true.B)
    dut.io.serial.sck.poke(false.B)
    dut.io.serial.sdi.poke(true.B)
    dut.clock.step()
    dut.io.serial.sck.poke(true.B)
    dut.clock.step()
  }

  protected def readBit(dut: EEPROM): Boolean = {
    dut.io.serial.sck.poke(false.B)
    dut.clock.step()
    dut.io.serial.sck.poke(true.B)
    dut.clock.step()
    dut.io.serial.sdo.peek().litToBoolean
  }

  protected def writeBit(dut: EEPROM, b: Bool) = {
    dut.io.serial.sck.poke(false.B)
    dut.io.serial.sdi.poke(b)
    dut.clock.step()
    dut.io.serial.sck.poke(true.B)
    dut.clock.step()
  }

  protected def readBits(dut: EEPROM): Seq[Boolean] = 0.to(15).map(_ => readBit(dut))
  protected def writeBits(dut: EEPROM, data: Bits): Unit = writeBits(dut, data.asBools.reverse)
  protected def writeBits(dut: EEPROM, bs: Seq[Bool]): Unit = bs.foreach(b => writeBit(dut, b))
  protected def read(dut: EEPROM, addr: Int) = command(dut, 2, addr)
  protected def write(dut: EEPROM, addr: Int) = command(dut, 1, addr)
  protected def erase(dut: EEPROM, addr: Int) = command(dut, 3, addr)
  protected def writeAll(dut: EEPROM) = command(dut, 0, 16)
  protected def eraseAll(dut: EEPROM) = command(dut, 0, 32)
  protected def enableWrite(dut: EEPROM) = command(dut, 0, 48)
  protected def disableWrite(dut: EEPROM) = command(dut, 0, 0)

  protected def command(dut: EEPROM, opcode: Int, addr: Int) = {
    waitForIdle(dut)
    startBit(dut)
    writeBits(dut, opcode.U(2.W))
    writeBits(dut, addr.U(6.W))
  }

  protected def waitForIdle(dut: EEPROM) =
    while (!dut.io.debug.idle.peek().litToBoolean) { dut.clock.step() }

  protected def waitForCommand(dut: EEPROM) =
    while (!dut.io.debug.command.peek().litToBoolean) { dut.clock.step() }

  protected def waitForRead(dut: EEPROM) =
    while (!dut.io.debug.read.peek().litToBoolean) { dut.clock.step() }
}

class EEPROMTest extends FlatSpec with ChiselScalatestTester with Matchers with EEPROMTestHelpers {
  behavior of "FSM"

  it should "move to the command state after receiving a start bit" in {
    test(new EEPROM) { dut =>
      startBit(dut)
      dut.io.debug.command.expect(true.B)
    }
  }

  it should "move to the read state after receiving a read command" in {
    test(new EEPROM) { dut =>
      read(dut, 0)
      dut.io.debug.read.expect(true.B)
    }
  }

  it should "move to the read wait state after a memory read request" in {
    test(new EEPROM) { dut =>
      read(dut, 0)
      dut.io.mem.waitReq.poke(true.B)
      dut.clock.step()
      dut.io.mem.waitReq.poke(false.B)
      dut.io.debug.readWait.expect(false.B)
      dut.clock.step()
      dut.io.debug.readWait.expect(true.B)
    }
  }

  it should "move to the shift state after the read wait state" in {
    test(new EEPROM) { dut =>
      read(dut, 0)
      dut.clock.step()
      dut.io.mem.valid.poke(true.B)
      dut.io.debug.shift.expect(false.B)
      dut.clock.step()
      dut.io.debug.shift.expect(true.B)
    }
  }

  it should "move to the idle state after sending serial data" in {
    test(new EEPROM) { dut =>
      read(dut, 0)
      dut.io.mem.valid.poke(true.B)
      dut.clock.step()
      readBits(dut)
      dut.io.debug.idle.expect(true.B)
    }
  }

  it should "move to the write state when writing is enabled" in {
    test(new EEPROM) { dut =>
      write(dut, 0)
      dut.io.debug.write.expect(false.B)
      enableWrite(dut)
      write(dut, 0)
      dut.io.debug.write.expect(true.B)
    }
  }

  it should "move to the erase state when writing is enabled" in {
    test(new EEPROM) { dut =>
      erase(dut, 0)
      dut.io.debug.erase.expect(false.B)
      enableWrite(dut)
      erase(dut, 0)
      dut.io.debug.erase.expect(true.B)
    }
  }

  it should "move to the shift state after the write state" in {
    test(new EEPROM) { dut =>
      enableWrite(dut)
      write(dut, 0)
      dut.clock.step()
      dut.io.debug.shift.expect(true.B)
    }
  }

  it should "move to the write wait state after receiving serial data" in {
    test(new EEPROM) { dut =>
      enableWrite(dut)
      write(dut, 0)
      dut.clock.step()
      dut.io.debug.writeWait.expect(false.B)
      writeBits(dut, 0.U(16.W))
      dut.io.debug.writeWait.expect(true.B)
    }
  }

  it should "move to the write wait state after the erase state" in {
    test(new EEPROM) { dut =>
      enableWrite(dut)
      erase(dut, 0)
      dut.clock.step()
      dut.io.debug.writeWait.expect(true.B)
    }
  }

  it should "move to the idle state after the write wait state" in {
    test(new EEPROM) { dut =>
      enableWrite(dut)
      erase(dut, 0)
      dut.clock.step()
      dut.io.mem.waitReq.poke(true.B)
      dut.clock.step()
      dut.io.mem.waitReq.poke(false.B)
      dut.io.debug.writeWait.expect(true.B)
      dut.clock.step()
      dut.io.debug.idle.expect(true.B)
    }
  }

  it should "move to the idle state when chip select is deasserted" in {
    test(new EEPROM) { dut =>
      read(dut, 0)
      dut.io.serial.cs.poke(false.B)
      dut.clock.step()
      dut.io.debug.idle.expect(true.B)
    }
  }

  behavior of "idle"

  it should "assert the serial data output" in {
    test(new EEPROM) { dut =>
      dut.clock.step()
      dut.io.serial.sdo.expect(true.B)
    }
  }

  behavior of "read"

  it should "read a word from memory" in {
    test(new EEPROM) { dut =>
      read(dut, 0x12)
      dut.io.mem.valid.poke(true.B)
      dut.io.mem.dout.poke(0x8421.U)
      dut.io.serial.sdo.expect(false.B)
      dut.io.mem.rd.expect(true.B)
      dut.io.mem.addr.expect(0x24.U)
      dut.clock.step()
      readBit(dut) shouldBe true
      readBit(dut) shouldBe false
      readBit(dut) shouldBe false
      readBit(dut) shouldBe false
      readBit(dut) shouldBe false
      readBit(dut) shouldBe true
      readBit(dut) shouldBe false
      readBit(dut) shouldBe false
      readBit(dut) shouldBe false
      readBit(dut) shouldBe false
      readBit(dut) shouldBe true
      readBit(dut) shouldBe false
      readBit(dut) shouldBe false
      readBit(dut) shouldBe false
      readBit(dut) shouldBe false
      readBit(dut) shouldBe true
    }
  }

  behavior of "write"

  it should "write a word to memory" in {
    test(new EEPROM) { dut =>
      enableWrite(dut)
      write(dut, 0x12)
      dut.clock.step()
      writeBits(dut, 0x1234.U(16.W))
      dut.io.mem.wr.expect(true.B)
      dut.io.mem.addr.expect(0x24.U)
      dut.io.mem.din.expect(0x1234.U)
    }
  }

  it should "write all words to memory" in {
    test(new EEPROM) { dut =>
      enableWrite(dut)
      writeAll(dut)
      dut.io.serial.sdo.expect(false.B)
      dut.clock.step()
      for (n <- 0 to 63) {
        writeBits(dut, n.U(16.W))
        dut.io.mem.wr.expect(true.B)
        dut.io.mem.addr.expect((n << 1).U)
        dut.io.mem.din.expect(n.U)
      }
    }
  }

  behavior of "erase"

  it should "erase a word in memory" in {
    test(new EEPROM) { dut =>
      enableWrite(dut)
      erase(dut, 0x12)
      dut.clock.step()
      dut.io.mem.wr.expect(true.B)
      dut.io.mem.addr.expect(0x24.U)
      dut.io.mem.din.expect(0xffff.U)
    }
  }

  it should "erase all words in memory" in {
    test(new EEPROM) { dut =>
      enableWrite(dut)
      eraseAll(dut)
      dut.io.serial.sdo.expect(false.B)
      for (n <- 0 to 63) {
        dut.clock.step()
        dut.io.mem.wr.expect(true.B)
        dut.io.mem.addr.expect((n << 1).U)
        dut.io.mem.din.expect(0xffff.U)
      }
    }
  }
}
