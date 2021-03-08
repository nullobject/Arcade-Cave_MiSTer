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

package axon

import chisel3._
import chiseltest._
import chiseltest.experimental.UncheckedClockPoke._
import org.scalatest._

class DataFreezerTest extends FlatSpec with ChiselScalatestTester with Matchers {
  private def mkFreezer = new ReadWriteDataFreezer(addrWidth = 8, dataWidth = 8)

  behavior of "read"

  it should "latch a read request" in {
    test(mkFreezer) { dut =>
      // Read
      dut.io.in.rd.poke(true.B)
      dut.io.out.waitReq.poke(true.B)
      dut.io.out.rd.expect(true.B)
      dut.clock.step()

      // Wait
      dut.io.out.waitReq.poke(false.B)
      dut.io.out.rd.expect(true.B)
      dut.clock.step()
      dut.io.out.waitReq.poke(true.B)
      dut.io.out.rd.expect(false.B)
      dut.clock.step()

      // Rising edge
      dut.io.targetClock.low()
      dut.io.targetClock.high()
      dut.io.out.rd.expect(false.B)
      dut.clock.step()

      // Valid
      dut.io.out.valid.poke(true.B)
      dut.io.out.rd.expect(false.B)
      dut.clock.step()
      dut.io.out.valid.poke(false.B)
      dut.io.out.rd.expect(false.B)

      // Rising edge
      dut.io.targetClock.low()
      dut.io.targetClock.high()
      dut.io.out.rd.expect(true.B)
    }
  }

  it should "latch valid output data" in {
    test(mkFreezer) { dut =>
      // Read
      dut.io.in.rd.poke(true.B)
      dut.io.out.valid.poke(true.B)
      dut.io.out.dout.poke(1.U)
      dut.clock.step()
      dut.io.out.dout.poke(0.U)
      dut.io.in.dout.expect(1.U)
      dut.clock.step()

      // Rising edge
      dut.io.targetClock.low()
      dut.io.targetClock.high()
      dut.io.in.dout.expect(0.U)
    }
  }

  behavior of "write"

  it should "latch a write request" in {
    test(mkFreezer) { dut =>
      // Write
      dut.io.in.wr.poke(true.B)
      dut.io.out.waitReq.poke(true.B)
      dut.io.out.wr.expect(true.B)
      dut.clock.step()

      // Wait
      dut.io.out.waitReq.poke(false.B)
      dut.io.out.wr.expect(true.B)
      dut.clock.step()
      dut.io.out.waitReq.poke(true.B)
      dut.io.out.wr.expect(false.B)
      dut.clock.step()

      // Rising edge
      dut.io.targetClock.low()
      dut.io.targetClock.high()
      dut.io.out.wr.expect(true.B)
    }
  }

  behavior of "wait"

  it should "latch the wait signal" in {
    test(mkFreezer) { dut =>
      // Assert wait
      dut.io.out.waitReq.poke(true.B)
      dut.io.in.waitReq.expect(true.B)
      dut.clock.step()

      // Deassert wait
      dut.io.out.waitReq.poke(false.B)
      dut.io.in.waitReq.expect(false.B)
      dut.clock.step()
      dut.io.out.waitReq.poke(true.B)
      dut.io.in.waitReq.expect(false.B)

      // Rising edge
      dut.io.targetClock.low()
      dut.io.targetClock.high()
      dut.io.in.waitReq.expect(true.B)
    }
  }

  behavior of "valid"

  it should "latch the valid signal" in {
    test(mkFreezer) { dut =>
      // Assert valid
      dut.io.out.valid.poke(true.B)
      dut.io.in.valid.expect(true.B)
      dut.clock.step()

      // Deassert valid
      dut.io.out.valid.poke(false.B)
      dut.io.in.valid.expect(true.B)
      dut.clock.step()
      dut.io.in.valid.expect(true.B)

      // Rising edge
      dut.io.targetClock.low()
      dut.io.targetClock.high()
      dut.io.in.valid.expect(false.B)
    }
  }
}
