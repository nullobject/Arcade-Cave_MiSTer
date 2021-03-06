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

package cave.gpu

import chisel3._
import chiseltest._
import org.scalatest._

class RegisterFileTest extends FlatSpec with ChiselScalatestTester with Matchers {
  it should "allow writing masked bytes" in {
    test(new RegisterFile(3)) { dut =>
      dut.io.mem.wr.poke(true.B)
      dut.io.mem.rd.poke(true.B)

      // Write
      dut.io.mem.mask.poke(0.U)
      dut.io.mem.din.poke(0x1234.U)
      dut.clock.step()
      dut.io.mem.dout.expect(0x0000.U)

      // Write
      dut.io.mem.mask.poke(1.U)
      dut.io.mem.din.poke(0x1234.U)
      dut.clock.step()
      dut.io.mem.dout.expect(0x0034.U)

      // Write
      dut.io.mem.mask.poke(2.U)
      dut.io.mem.din.poke(0x5678.U)
      dut.clock.step()
      dut.io.mem.dout.expect(0x5634.U)

      // Write
      dut.io.mem.mask.poke(3.U)
      dut.io.mem.din.poke(0xabcd.U)
      dut.clock.step()
      dut.io.mem.dout.expect(0xabcd.U)
    }
  }

  it should "output the registers" in {
    test(new RegisterFile(3)) { dut =>
      dut.io.mem.wr.poke(true.B)
      dut.io.mem.mask.poke(3.U)

      // Write
      dut.io.mem.addr.poke(0.U)
      dut.io.mem.din.poke(0x1234.U)
      dut.clock.step()
      dut.io.mem.addr.poke(1.U)
      dut.io.mem.din.poke(0x5678.U)
      dut.clock.step()
      dut.io.mem.addr.poke(2.U)
      dut.io.mem.din.poke(0xabcd.U)
      dut.clock.step()

      // Read
      dut.io.regs(0).expect(0x1234.U)
      dut.io.regs(1).expect(0x5678.U)
      dut.io.regs(2).expect(0xabcd.U)
    }
  }
}
