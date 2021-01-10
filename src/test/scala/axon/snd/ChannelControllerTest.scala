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

package axon.snd

import chisel3._
import chiseltest._
import org.scalatest._

trait ChannelControllerTestHelpers {
  protected val ymzConfig = YMZ280BConfig(clockFreq = 44100 * 50, numChannels = 1)

  protected def mkChannelController(config: YMZ280BConfig = ymzConfig) = new ChannelController(config)

  protected def startChannel(dut: ChannelController,
                             index: Int,
                             loop: Boolean = false,
                             pitch: Int = 255,
                             level: Int = 255,
                             pan: Int = 7,
                             startAddress: Int = 0,
                             loopStartAddr: Int = 0,
                             loopEndAddr: Int = 0,
                             endAddress: Int = 0) = {
    dut.io.regs(index).flags.keyOn.poke(true.B)
    dut.io.regs(index).flags.loop.poke(loop.B)
    dut.io.regs(index).pitch.poke(pitch.U)
    dut.io.regs(index).level.poke(level.U)
    dut.io.regs(index).pan.poke(pan.U)
    dut.io.regs(index).startAddr.poke(startAddress.U)
    dut.io.regs(index).loopStartAddr.poke(loopStartAddr.U)
    dut.io.regs(index).loopEndAddr.poke(loopEndAddr.U)
    dut.io.regs(index).endAddr.poke(endAddress.U)
  }

  protected def stopChannel(dut: ChannelController, index: Int) = {
    dut.io.regs(index).flags.keyOn.poke(false.B)
  }

  protected def waitForIdle(dut: ChannelController) =
    while (!dut.io.debug.idle.peek().litToBoolean) { dut.clock.step() }

  protected def waitForRead(dut: ChannelController) =
    while (!dut.io.debug.read.peek().litToBoolean) { dut.clock.step() }

  protected def waitForLatch(dut: ChannelController) =
    while (!dut.io.debug.latch.peek().litToBoolean) { dut.clock.step() }

  protected def waitForCheck(dut: ChannelController) =
    while (!dut.io.debug.check.peek().litToBoolean) { dut.clock.step() }

  protected def waitForReady(dut: ChannelController) =
    while (!dut.io.debug.ready.peek().litToBoolean) { dut.clock.step() }

  protected def waitForProcess(dut: ChannelController) =
    while (!dut.io.debug.process.peek().litToBoolean) { dut.clock.step() }

  protected def waitForWrite(dut: ChannelController) =
    while (!dut.io.debug.write.peek().litToBoolean) { dut.clock.step() }

  protected def waitForNext(dut: ChannelController) =
    while (!dut.io.debug.next.peek().litToBoolean) { dut.clock.step() }

  protected def waitForDone(dut: ChannelController) =
    while (!dut.io.debug.done.peek().litToBoolean) { dut.clock.step() }

  protected def waitForMemRead(dut: ChannelController) =
    while (!dut.io.mem.rd.peek().litToBoolean) { dut.clock.step() }

  protected def waitForAudioValid(dut: ChannelController) =
    while (!dut.io.audio.valid.peek().litToBoolean) { dut.clock.step() }

  protected def waitForActive(dut: ChannelController, active: Boolean = true) = {
    waitForCheck(dut)
    if (active) {
      while (!dut.io.active.peek().litToBoolean) {
        dut.clock.step()
        waitForCheck(dut)
      }
    } else {
      while (dut.io.active.peek().litToBoolean) {
        dut.clock.step()
        waitForCheck(dut)
      }
    }
  }
}

class ChannelControllerTest extends FlatSpec with ChiselScalatestTester with Matchers with ChannelControllerTestHelpers {
  behavior of "FSM"

  it should "remain in the init state when the channel controller is disabled" in {
    test(mkChannelController()) { dut =>
      waitForIdle(dut)
      dut.clock.step()
      dut.io.enable.poke(true.B)
      dut.io.debug.idle.expect(true.B)
      dut.clock.step()
      dut.io.debug.idle.expect(false.B)
    }
  }

  it should "move to the read state after initializing" in {
    test(mkChannelController()) { dut =>
      dut.io.enable.poke(true.B)
      waitForIdle(dut)
      dut.clock.step()
      dut.io.debug.read.expect(true.B)
    }
  }

  it should "move to the latch state after reading the channel state" in {
    test(mkChannelController()) { dut =>
      dut.io.enable.poke(true.B)
      waitForRead(dut)
      dut.clock.step()
      dut.io.debug.latch.expect(true.B)
    }
  }

  it should "move to the check state after latching the channel state" in {
    test(mkChannelController()) { dut =>
      dut.io.enable.poke(true.B)
      waitForLatch(dut)
      dut.clock.step()
      dut.io.debug.check.expect(true.B)
    }
  }

  it should "move to the ready state after checking an enabled channel" in {
    test(mkChannelController()) { dut =>
      dut.io.enable.poke(true.B)
      dut.io.regs(0).flags.keyOn.poke(true.B)
      waitForCheck(dut)
      dut.clock.step()
      dut.io.debug.ready.expect(true.B)
    }
  }

  it should "move to the write state after checking a disabled channel" in {
    test(mkChannelController()) { dut =>
      dut.io.enable.poke(true.B)
      waitForCheck(dut)
      dut.clock.step()
      dut.io.debug.write.expect(true.B)
    }
  }

  it should "move to the done state after processing all channels" in {
    test(mkChannelController()) { dut =>
      dut.io.enable.poke(true.B)
      waitForNext(dut)
      dut.clock.step()
      waitForNext(dut)
      dut.clock.step()
      dut.io.debug.done.expect(true.B)
    }
  }

  it should "move to the idle state after setting the audio output" in {
    test(mkChannelController()) { dut =>
      dut.io.enable.poke(true.B)
      waitForAudioValid(dut)
      dut.clock.step()
      dut.io.debug.idle.expect(true.B)
    }
  }

  behavior of "PCM data"

  it should "assert the memory read enable signal" in {
    test(mkChannelController()) { dut =>
      // Start
      dut.io.enable.poke(true.B)
      startChannel(dut, index = 0)

      // Fetch
      waitForProcess(dut)
      dut.clock.step()
      dut.io.mem.waitReq.poke(true.B)
      dut.io.mem.rd.expect(true.B)
      dut.clock.step()
      dut.io.mem.waitReq.poke(false.B)
      dut.io.mem.rd.expect(true.B)
      dut.clock.step()
      dut.io.mem.rd.expect(false.B)
    }
  }

  it should "fetch PCM data for the channels" in {
    test(mkChannelController()) { dut =>
      // Start
      dut.io.enable.poke(true.B)
      dut.io.mem.valid.poke(true.B)
      startChannel(dut, index = 0, pitch = 127, startAddress = 1)

      for (n <- Seq(1, 1, 2, 2)) {
        // Fetch
        waitForProcess(dut)
        dut.clock.step()
        dut.io.mem.rd.expect(true.B)
        dut.io.mem.addr.expect(n.U)
        waitForNext(dut)

        // No fetch
        waitForProcess(dut)
        dut.io.mem.rd.expect(false.B)
        waitForNext(dut)
      }
    }
  }

  it should "reset the address nibble when restarting a channel" in {
    test(mkChannelController()) { dut =>
      // Start
      dut.io.enable.poke(true.B)
      dut.io.mem.valid.poke(true.B)
      startChannel(dut, index = 0, pitch = 127)

      // Process
      waitForProcess(dut)
      dut.io.debug.channelReg.nibble.expect(false.B)
      waitForNext(dut)
      dut.io.debug.channelReg.nibble.expect(true.B)

      // Stop
      waitForIdle(dut)
      stopChannel(dut, index = 0)

      // Inactive
      waitForActive(dut, false)

      // Start
      waitForIdle(dut)
      startChannel(dut, index = 0, pitch = 127)

      // Process
      waitForProcess(dut)
      dut.io.debug.channelReg.nibble.expect(false.B)
      waitForNext(dut)
      dut.io.debug.channelReg.nibble.expect(true.B)
    }
  }

  behavior of "playing"

  it should "play a looped sample" in {
    test(mkChannelController()) { dut =>
      // Start
      dut.io.enable.poke(true.B)
      startChannel(dut, index = 0, loop = true, loopEndAddr = 1, endAddress = 1)

      // Fetch
      waitForMemRead(dut)
      dut.io.mem.valid.poke(true.B)
      dut.io.mem.dout.poke(0x12.U)
      waitForNext(dut)
      dut.io.mem.valid.poke(false.B)

      // Valid
      waitForAudioValid(dut)
      dut.io.audio.bits.left.expect(0.S)
      dut.io.audio.bits.right.expect(0.S)
      waitForIdle(dut)

      // Fetch
      waitForMemRead(dut)
      dut.io.mem.valid.poke(true.B)
      dut.io.mem.dout.poke(0x12.U)
      waitForNext(dut)
      dut.io.mem.valid.poke(false.B)

      // Valid
      waitForAudioValid(dut)
      dut.io.audio.bits.left.expect(23.S)
      dut.io.audio.bits.right.expect(23.S)
      waitForIdle(dut)

      // Fetch
      waitForMemRead(dut)
      dut.io.mem.valid.poke(true.B)
      dut.io.mem.dout.poke(0x12.U)
      waitForNext(dut)
      dut.io.mem.valid.poke(false.B)

      // Valid
      waitForAudioValid(dut)
      dut.io.audio.bits.left.expect(63.S)
      dut.io.audio.bits.right.expect(63.S)
      waitForIdle(dut)

      // Fetch
      waitForMemRead(dut)
      dut.io.mem.valid.poke(true.B)
      dut.io.mem.dout.poke(0x12.U)
      waitForNext(dut)
      dut.io.mem.valid.poke(false.B)

      // Valid
      waitForAudioValid(dut)
      dut.io.audio.bits.left.expect(86.S)
      dut.io.audio.bits.right.expect(86.S)
      waitForIdle(dut)

      // Fetch
      waitForMemRead(dut)
      dut.io.mem.valid.poke(true.B)
      dut.io.mem.dout.poke(0x12.U)
      waitForNext(dut)
      dut.io.mem.valid.poke(false.B)

      // Valid
      waitForAudioValid(dut)
      dut.io.audio.bits.left.expect(126.S)
      dut.io.audio.bits.right.expect(126.S)
      waitForIdle(dut)

      // Fetch
      waitForMemRead(dut)
      dut.io.mem.valid.poke(true.B)
      dut.io.mem.dout.poke(0x12.U)
      waitForNext(dut)
      dut.io.mem.valid.poke(false.B)

      // Valid
      waitForAudioValid(dut)
      dut.io.audio.bits.left.expect(23.S)
      dut.io.audio.bits.right.expect(23.S)
    }
  }

  it should "sum the channel outputs" in {
    test(mkChannelController(ymzConfig.copy(numChannels = 2))) { dut =>
      // Start
      dut.io.enable.poke(true.B)
      startChannel(dut, index = 0, pitch = 127)
      startChannel(dut, index = 1, pitch = 127)

      // Fetch (channel 0)
      waitForMemRead(dut)
      dut.io.mem.valid.poke(true.B)
      dut.io.mem.dout.poke(0x10.U)
      dut.io.index.expect(0.U)
      waitForNext(dut)
      dut.io.mem.valid.poke(false.B)

      // Fetch (channel 1)
      waitForMemRead(dut)
      dut.io.mem.valid.poke(true.B)
      dut.io.mem.dout.poke(0x20.U)
      dut.io.index.expect(1.U)
      waitForNext(dut)
      dut.io.mem.valid.poke(false.B)

      // Valid
      waitForAudioValid(dut)
      dut.io.audio.bits.left.expect(0.S)
      dut.io.audio.bits.right.expect(0.S)
      waitForIdle(dut)

      // Valid
      waitForAudioValid(dut)
      dut.io.audio.bits.left.expect(30.S)
      dut.io.audio.bits.right.expect(30.S)
    }
  }

  it should "not allow a channel to play another sample until the key is released" in {
    test(mkChannelController()) { dut =>
      // Start
      dut.io.enable.poke(true.B)
      dut.io.mem.valid.poke(true.B)
      startChannel(dut, index = 0, endAddress = 1)

      // Done
      for (_ <- 0 to 4) {
        waitForRead(dut)
        waitForCheck(dut)
      }
      dut.io.active.expect(false.B)
      dut.io.done.expect(true.B)

      // Key still down
      waitForRead(dut)
      waitForCheck(dut)
      dut.io.active.expect(false.B)

      // Stop
      stopChannel(dut, index = 0)
      waitForRead(dut)
      waitForCheck(dut)

      // Start
      startChannel(dut, index = 0, endAddress = 1)
      waitForRead(dut)
      waitForCheck(dut)
      dut.io.active.expect(true.B)
    }
  }

  behavior of "active"

  it should "assert the active signal when a channel is playing" in {
    test(mkChannelController()) { dut =>
      dut.io.enable.poke(true.B)

      // Active
      startChannel(dut, index = 0, endAddress = 1)
      waitForCheck(dut)
      dut.io.active.expect(true.B)

      // Inactive
      stopChannel(dut, index = 0)
      waitForCheck(dut)
      dut.io.active.expect(false.B)
    }
  }

  it should "deassert the active signal when a channel has reached the end address" in {
    test(mkChannelController()) { dut =>
      // Start
      dut.io.enable.poke(true.B)
      dut.io.mem.valid.poke(true.B)
      startChannel(dut, index = 0, endAddress = 1)

      // Status
      for (_ <- 0 to 3) {
        waitForRead(dut)
        waitForCheck(dut)
        dut.io.active.expect(true.B)
      }

      // Done
      waitForRead(dut)
      waitForCheck(dut)
      dut.io.active.expect(false.B)
    }
  }

  behavior of "done"

  it should "assert the done signal when a channel has reached the end address" in {
    test(mkChannelController()) { dut =>
      // Start
      dut.io.enable.poke(true.B)
      dut.io.mem.valid.poke(true.B)
      startChannel(dut, index = 0, endAddress = 1)

      // Status
      for (_ <- 0 to 3) {
        waitForRead(dut)
        waitForCheck(dut)
        dut.io.done.expect(false.B)
      }

      // Done
      waitForRead(dut)
      waitForCheck(dut)
      dut.io.done.expect(true.B)
    }
  }

  it should "not assert the done signal when the loop end address is equal to the end address" in {
    test(mkChannelController()) { dut =>
      // Start
      dut.io.enable.poke(true.B)
      dut.io.mem.valid.poke(true.B)
      startChannel(dut, index = 0, loop = true, loopEndAddr = 1, endAddress = 1)

      // Status
      for (n <- Seq(0, 1, 1, 0)) {
        waitForRead(dut)
        waitForCheck(dut)
        dut.io.done.expect(false.B)
      }
    }
  }

  it should "not assert the done signal when a channel is stopped" in {
    test(mkChannelController()) { dut =>
      // Start
      dut.io.enable.poke(true.B)
      dut.io.mem.valid.poke(true.B)
      startChannel(dut, index = 0, endAddress = 1)

      // Stop
      waitForNext(dut)
      stopChannel(dut, index = 0)

      // Status
      waitForCheck(dut)
      dut.io.done.expect(false.B)
    }
  }

  it should "not assert the done signal when a channel is inactive" in {
    test(mkChannelController()) { dut =>
      // Start
      dut.io.enable.poke(true.B)
      dut.io.mem.valid.poke(true.B)
      startChannel(dut, index = 0, endAddress = 1)

      // Done
      for (_ <- 0 to 4) {
        waitForRead(dut)
        waitForCheck(dut)
      }
      dut.io.done.expect(true.B)

      // Disabled
      waitForRead(dut)
      waitForCheck(dut)
      dut.io.done.expect(false.B)
    }
  }
}
