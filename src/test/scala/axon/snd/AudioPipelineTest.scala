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

trait AudioPipelineTestHelpers {
  protected val config = YMZ280BConfig(clockFreq = 44100, numChannels = 2)

  protected def mkChannelPipeline = new AudioPipeline(config)

  protected def startPipeline(dut: AudioPipeline,
                              samples: (Int, Int) = (0, 0),
                              underflow: Boolean = false,
                              lerpIndex: Int = 0,
                              adpcmStep: Int = 127,
                              pitch: Int = 255,
                              level: Int = 255,
                              pan: Int = 8,
                              zero: Boolean = false) = {
    waitForIdle(dut)
    dut.io.in.valid.poke(true.B)
    dut.io.in.bits.state.samples(0).poke(samples._1.S)
    dut.io.in.bits.state.samples(1).poke(samples._2.S)
    dut.io.in.bits.state.adpcmStep.poke(adpcmStep.S)
    dut.io.in.bits.state.lerpIndex.poke(lerpIndex.U)
    dut.io.in.bits.state.underflow.poke(underflow.B)
    dut.io.in.bits.pitch.poke(pitch.U)
    dut.io.in.bits.level.poke(level.U)
    dut.io.in.bits.pan.poke(pan.U)
    dut.io.in.bits.zero.poke(zero.B)
  }

  protected def waitForIdle(dut: AudioPipeline) =
    while (!dut.io.debug.idle.peek().litToBoolean) { dut.clock.step() }

  protected def waitForCheck(dut: AudioPipeline) =
    while (!dut.io.debug.check.peek().litToBoolean) { dut.clock.step() }

  protected def waitForFetch(dut: AudioPipeline) =
    while (!dut.io.debug.fetch.peek().litToBoolean) { dut.clock.step() }

  protected def waitForDecode(dut: AudioPipeline) =
    while (!dut.io.debug.decode.peek().litToBoolean) { dut.clock.step() }

  protected def waitForZero(dut: AudioPipeline) =
    while (!dut.io.debug.zero.peek().litToBoolean) { dut.clock.step() }

  protected def waitForInterpolate(dut: AudioPipeline) =
    while (!dut.io.debug.interpolate.peek().litToBoolean) { dut.clock.step() }

  protected def waitForLevel(dut: AudioPipeline) =
    while (!dut.io.debug.level.peek().litToBoolean) { dut.clock.step() }

  protected def waitForPan(dut: AudioPipeline) =
    while (!dut.io.debug.pan.peek().litToBoolean) { dut.clock.step() }

  protected def waitForDone(dut: AudioPipeline) =
    while (!dut.io.debug.done.peek().litToBoolean) { dut.clock.step() }
}

class AudioPipelineTest extends FlatSpec with ChiselScalatestTester with Matchers with AudioPipelineTestHelpers {
  behavior of "FSM"

  it should "assert the ready signal during the idle state" in {
    test(mkChannelPipeline) { dut =>
      dut.io.in.valid.poke(true.B)
      waitForIdle(dut)
      dut.io.in.ready.expect(true.B)
      dut.clock.step()
      dut.io.in.ready.expect(false.B)
    }
  }

  it should "move to the check state after receiving a request" in {
    test(mkChannelPipeline) { dut =>
      dut.io.in.valid.poke(true.B)
      waitForIdle(dut)
      dut.clock.step()
      dut.io.debug.check.expect(true.B)
    }
  }

  it should "move to the interpolate state when the pipeline has not underflowed" in {
    test(mkChannelPipeline) { dut =>
      dut.io.in.valid.poke(true.B)
      waitForCheck(dut)
      dut.clock.step()
      dut.io.debug.interpolate.expect(true.B)
    }
  }

  it should "move to the zero state when the pipeline underflows while it is fading out" in {
    test(mkChannelPipeline) { dut =>
      dut.io.in.valid.poke(true.B)
      dut.io.in.bits.zero.poke(true.B)
      dut.io.in.bits.state.underflow.poke(true.B)
      waitForCheck(dut)
      dut.clock.step()
      dut.io.debug.zero.expect(true.B)
    }
  }

  it should "move to the latch state when the pipeline underflows" in {
    test(mkChannelPipeline) { dut =>
      dut.io.in.valid.poke(true.B)
      dut.io.in.bits.state.underflow.poke(true.B)
      waitForCheck(dut)
      dut.clock.step()
      dut.io.debug.fetch.expect(true.B)
    }
  }

  it should "move to the level state after interpolating the sample" in {
    test(mkChannelPipeline) { dut =>
      dut.io.in.valid.poke(true.B)
      waitForInterpolate(dut)
      dut.clock.step()
      dut.io.debug.level.expect(true.B)
    }
  }

  it should "move to the pan state after applying the level" in {
    test(mkChannelPipeline) { dut =>
      dut.io.in.valid.poke(true.B)
      waitForLevel(dut)
      dut.clock.step()
      dut.io.debug.pan.expect(true.B)
    }
  }

  it should "move to the done state after applying the pan" in {
    test(mkChannelPipeline) { dut =>
      dut.io.in.valid.poke(true.B)
      waitForPan(dut)
      dut.clock.step()
      dut.io.debug.done.expect(true.B)
    }
  }

  it should "assert the valid signal during the done state" in {
    test(mkChannelPipeline) { dut =>
      dut.io.in.valid.poke(true.B)
      dut.io.out.valid.expect(false.B)
      waitForDone(dut)
      dut.io.out.valid.expect(true.B)
    }
  }

  behavior of "pipeline"

  it should "fetch ADPCM data when the pipeline underflows" in {
    test(mkChannelPipeline) { dut =>
      startPipeline(dut, underflow = true, lerpIndex = 64, pitch = 63)
      dut.io.pcmData.ready.expect(false.B)
      waitForFetch(dut)
      dut.io.pcmData.ready.expect(true.B)
    }
  }

  it should "not fetch ADPCM data when the pipeline has not underflowed" in {
    test(mkChannelPipeline) { dut =>
      startPipeline(dut)
      dut.io.pcmData.ready.expect(false.B)
      waitForCheck(dut)
      dut.io.pcmData.ready.expect(false.B)
    }
  }

  it should "update linear interpolation index and set underflow flag" in {
    test(mkChannelPipeline) { dut =>
      startPipeline(dut, pitch = 127)
      waitForDone(dut)
      dut.io.out.bits.state.underflow.expect(false.B)
      dut.io.out.bits.state.lerpIndex.expect(128.U)

      startPipeline(dut, pitch = 127, lerpIndex = 128)
      waitForDone(dut)
      dut.io.out.bits.state.underflow.expect(true.B)
      dut.io.out.bits.state.lerpIndex.expect(0.U)
    }
  }

  it should "decode ADPCM data" in {
    test(mkChannelPipeline) { dut =>
      // Start
      startPipeline(dut, underflow = true, lerpIndex = 64)
      dut.io.pcmData.ready.expect(false.B)

      // Data
      waitForFetch(dut)
      dut.io.pcmData.valid.poke(true.B)
      dut.io.pcmData.bits.poke(8.U)

      // Valid
      waitForDone(dut)
      dut.io.out.bits.audio.left.expect(-2.S)
      dut.io.out.bits.audio.right.expect(-2.S)
    }
  }

  it should "interpolate to zero" in {
    test(mkChannelPipeline) { dut =>
      startPipeline(dut, samples = (16, 16), underflow = true, lerpIndex = 128, zero = true)
      waitForDone(dut)
      dut.io.out.bits.audio.left.expect(4.S)
      dut.io.out.bits.audio.right.expect(4.S)
    }
  }

  it should "apply level" in {
    test(mkChannelPipeline) { dut =>
      startPipeline(dut, samples = (16, 0), level = 255)
      waitForDone(dut)
      dut.io.out.bits.audio.left.expect(8.S)
      dut.io.out.bits.audio.right.expect(8.S)

      startPipeline(dut, samples = (16, 0), level = 128)
      waitForDone(dut)
      dut.io.out.bits.audio.left.expect(4.S)
      dut.io.out.bits.audio.right.expect(4.S)

      startPipeline(dut, samples = (16, 0), level = 64)
      waitForDone(dut)
      dut.io.out.bits.audio.left.expect(2.S)
      dut.io.out.bits.audio.right.expect(2.S)

      startPipeline(dut, samples = (16, 0), level = 0)
      waitForDone(dut)
      dut.io.out.bits.audio.left.expect(0.S)
      dut.io.out.bits.audio.right.expect(0.S)
    }
  }

  it should "apply pan" in {
    test(mkChannelPipeline) { dut =>
      startPipeline(dut, samples = (16, 0), pan = 0)
      waitForDone(dut)
      dut.io.out.bits.audio.left.expect(8.S)
      dut.io.out.bits.audio.right.expect(0.S)

      startPipeline(dut, samples = (16, 0), pan = 1)
      waitForDone(dut)
      dut.io.out.bits.audio.left.expect(8.S)
      dut.io.out.bits.audio.right.expect(1.S)

      startPipeline(dut, samples = (16, 0), pan = 6)
      waitForDone(dut)
      dut.io.out.bits.audio.left.expect(8.S)
      dut.io.out.bits.audio.right.expect(6.S)

      startPipeline(dut, samples = (16, 0), pan = 7)
      waitForDone(dut)
      dut.io.out.bits.audio.left.expect(8.S)
      dut.io.out.bits.audio.right.expect(8.S)

      startPipeline(dut, samples = (16, 0), pan = 8)
      waitForDone(dut)
      dut.io.out.bits.audio.left.expect(8.S)
      dut.io.out.bits.audio.right.expect(8.S)

      startPipeline(dut, samples = (16, 0), pan = 9)
      waitForDone(dut)
      dut.io.out.bits.audio.left.expect(6.S)
      dut.io.out.bits.audio.right.expect(8.S)

      startPipeline(dut, samples = (16, 0), pan = 14)
      waitForDone(dut)
      dut.io.out.bits.audio.left.expect(1.S)
      dut.io.out.bits.audio.right.expect(8.S)

      startPipeline(dut, samples = (16, 0), pan = 15)
      waitForDone(dut)
      dut.io.out.bits.audio.left.expect(0.S)
      dut.io.out.bits.audio.right.expect(8.S)
    }
  }
}
