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
import chisel3.util._

/**
 * Produces audio samples by processing PCM data using a sequence of steps.
 *
 * The audio pipeline steps are:
 *
 *   - Check whether PCM data is required
 *   - Fetch PCM data
 *   - Decode ADPCM sample
 *   - Interpolate sample values
 *   - Apply level
 *   - Apply pan
 *   - Output audio sample
 *
 * @param config The YMZ280B configuration.
 */
class AudioPipeline(config: YMZ280BConfig) extends Module {
  val io = IO(new Bundle {
    /** Input port */
    val in = DeqIO(new Bundle {
      /** Pipeline state */
      val state = new AudioPipelineState(config)
      /** Pitch */
      val pitch = UInt(config.pitchWidth.W)
      /** Level */
      val level = UInt(config.levelWidth.W)
      /** Pan */
      val pan = UInt(config.panWidth.W)
      /** Interpolate to zero */
      val zero = Bool()
    })
    /** Output port */
    val out = ValidIO(new Bundle {
      /** Pipeline state */
      val state = new AudioPipelineState(config)
      /** Audio */
      val audio = new Audio(config.internalSampleWidth)
    })
    /** PCM data port */
    val pcmData = DeqIO(Bits(config.adpcmDataWidth.W))
    /** Asserted when the pipeline is at the start of a loop */
    val loopStart = Input(Bool())
    /** Debug port */
    val debug = new Bundle {
      val idle = Output(Bool())
      val check = Output(Bool())
      val fetch = Output(Bool())
      val decode = Output(Bool())
      val zero = Output(Bool())
      val interpolate = Output(Bool())
      val level = Output(Bool())
      val pan = Output(Bool())
      val done = Output(Bool())
    }
  })

  // States
  object State {
    val idle :: check :: fetch :: decode :: zero :: interpolate :: level :: pan :: done :: Nil = Enum(9)
  }

  // Registers
  val stateReg = RegInit(State.idle)
  val inputReg = RegEnable(io.in.bits, io.in.fire())
  val sampleReg = Reg(SInt(config.internalSampleWidth.W))
  val audioReg = Reg(new Audio(config.internalSampleWidth))
  val pcmDataReg = RegEnable(io.pcmData.bits, io.pcmData.fire())

  // ADPCM decoder
  val adpcm = Module(new ADPCM(
    sampleWidth = config.internalSampleWidth,
    dataWidth = config.adpcmDataWidth
  ))
  adpcm.io.data := pcmDataReg
  adpcm.io.in.step := inputReg.state.adpcmStep
  adpcm.io.in.sample := inputReg.state.samples(1)

  // Linear interpolator
  val lerp = Module(new LERP(
    sampleWidth = config.internalSampleWidth,
    indexWidth = config.lerpIndexWidth
  ))
  lerp.io.samples := inputReg.state.samples
  lerp.io.index := inputReg.state.lerpIndex

  // When the loop start flag is asserted the pipeline will use the cached step and sample values
  // for the start of the loop. Otherwise, the decoded ADPCM step and sample values will be used
  // instead.
  when(stateReg === State.decode) {
    when(io.loopStart && !inputReg.state.loopEnable) {
      inputReg.state.loopEnable := true.B
      inputReg.state.loopStep := adpcm.io.out.step
      inputReg.state.loopSample := adpcm.io.out.sample
    }
    val step = Mux(io.loopStart && inputReg.state.loopEnable, inputReg.state.loopStep, adpcm.io.out.step)
    val sample = Mux(io.loopStart && inputReg.state.loopEnable, inputReg.state.loopSample, adpcm.io.out.sample)
    inputReg.state.adpcm(step, sample)
  }

  // When fading out we just add an empty ADPCM value so the pipeline will interpolate to zero
  when(stateReg === State.zero) {
    inputReg.state.adpcm(127.S, 0.S)
  }

  // Interpolate sample values
  when(stateReg === State.interpolate) {
    inputReg.state.interpolate(inputReg.pitch)
    sampleReg := lerp.io.out
  }

  // Apply level
  when(stateReg === State.level) {
    sampleReg := sampleReg * (inputReg.level +& 1.U) >> 9
  }

  // Apply pan
  when(stateReg === State.pan) {
    val s = sampleReg
    val t = inputReg.pan(2, 0)
    val left = WireInit(s)
    val right = WireInit(s)
    when(inputReg.pan > 8.U) {
      left := s * (~t).asUInt >> 3
    }.elsewhen(inputReg.pan < 7.U) {
      right := s * t >> 3
    }
    audioReg := Audio(left, right)(config.internalSampleWidth)
  }

  // FSM
  switch(stateReg) {
    // Wait for a request
    is(State.idle) {
      when(io.in.valid) { stateReg := State.check }
    }

    // Check whether PCM data is required
    is(State.check) {
      stateReg := MuxCase(State.interpolate, Seq(
        (inputReg.state.underflow && inputReg.zero) -> State.zero,
        inputReg.state.underflow -> State.fetch
      ))
    }

    // Fetch sample data
    is(State.fetch) {
      when(io.pcmData.valid) { stateReg := State.decode }
    }

    // Decode ADPCM sample
    is(State.decode) { stateReg := State.interpolate }

    // Insert zero sample
    is(State.zero) { stateReg := State.interpolate }

    // Interpolate sample
    is(State.interpolate) { stateReg := State.level }

    // Apply level
    is(State.level) { stateReg := State.pan }

    // Apply pan
    is(State.pan) { stateReg := State.done }

    // Done
    is(State.done) { stateReg := State.idle }
  }

  // Outputs
  io.in.ready := stateReg === State.idle
  io.out.valid := stateReg === State.done
  io.out.bits.state := inputReg.state
  io.out.bits.audio := audioReg
  io.pcmData.ready := stateReg === State.fetch
  io.debug.idle := stateReg === State.idle
  io.debug.check := stateReg === State.check
  io.debug.fetch := stateReg === State.fetch
  io.debug.decode := stateReg === State.decode
  io.debug.zero := stateReg === State.zero
  io.debug.interpolate := stateReg === State.interpolate
  io.debug.level := stateReg === State.level
  io.debug.pan := stateReg === State.pan
  io.debug.done := stateReg === State.done

//  printf(p"AudioPipeline(state: $stateReg, pcmData: 0x${ Hexadecimal(pcmDataReg) }, pipelineState: ${ inputReg.state })\n")
}
