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

import axon.Util
import axon.mem.AsyncReadMemIO
import axon.util.Counter
import chisel3._
import chisel3.util._

/**
 * Manages the channels.
 *
 * The channel controller processes each channel through the audio pipeline and sums their audio
 * outputs.
 *
 * @param config The YMZ280B configuration.
 */
class ChannelController(config: YMZ280BConfig) extends Module {
  val io = IO(new Bundle {
    /** Channel registers port */
    val regs = Input(Vec(config.numChannels, new ChannelReg))
    /** A flag indicating whether the channel controller is enabled */
    val enable = Input(Bool())
    /** Asserted when the current channel is active */
    val active = Output(Bool())
    /** Asserted when the current channel is done */
    val done = Output(Bool())
    /** Current channel index */
    val index = Output(UInt())
    /** Audio output port */
    val audio = ValidIO(new Audio(config.sampleWidth))
    /** External memory port */
    val mem = AsyncReadMemIO(config.memAddrWidth, config.memDataWidth)
    /** Debug port */
    val debug = new Bundle {
      val init = Output(Bool())
      val idle = Output(Bool())
      val read = Output(Bool())
      val latch = Output(Bool())
      val check = Output(Bool())
      val ready = Output(Bool())
      val process = Output(Bool())
      val write = Output(Bool())
      val next = Output(Bool())
      val done = Output(Bool())
      val channelReg = Output(new ChannelState(config))
    }
  })

  // States
  object State {
    val init :: idle :: read :: latch :: check :: ready :: process :: write :: next :: done :: Nil = Enum(10)
  }

  // Registers
  val stateReg = RegInit(State.init)
  val accumulatorReg = Reg(new Audio(config.internalSampleWidth))

  // Counters
  val (channelCounter, channelCounterWrap) = Counter.static(config.numChannels, enable = stateReg === State.init || stateReg === State.next)
  val (_, outputCounterWrap) = Counter.static((config.clockFreq / config.sampleFreq).round.toInt)

  // Register aliases
  val channelReg = io.regs(channelCounter)

  // Channel state memory
  val channelStateMem = SyncReadMem(config.numChannels, Bits(new ChannelState(config).getWidth.W))
  val channelState = channelStateMem.read(channelCounter, stateReg === State.read).asTypeOf(new ChannelState(config))
  val channelStateReg = RegEnable(channelState, stateReg === State.latch)

  // Audio pipeline
  val audioPipeline = Module(new AudioPipeline(config))
  audioPipeline.io.in.valid := stateReg === State.ready
  audioPipeline.io.in.bits.state := channelStateReg.audioPipelineState
  audioPipeline.io.in.bits.pitch := channelReg.pitch
  audioPipeline.io.in.bits.level := channelReg.level
  audioPipeline.io.in.bits.pan := channelReg.pan
  audioPipeline.io.in.bits.fadeOut := channelStateReg.fadeOut
  audioPipeline.io.pcmData.valid := io.mem.valid
  audioPipeline.io.pcmData.bits := Mux(channelStateReg.nibble, io.mem.dout(3, 0), io.mem.dout(7, 4))
  audioPipeline.io.loopStart := channelStateReg.loopStart

  // Control signals
  val start = !channelStateReg.keyOn && channelReg.flags.keyOn && !channelStateReg.active
  val stop = channelStateReg.keyOn && !channelReg.flags.keyOn
  val active = channelStateReg.active || start
  val zero = channelStateReg.active && channelStateReg.fadeOut && channelStateReg.audioPipelineState.isZero
  val done = zero && channelStateReg.complete

  // Fetch PCM data when the audio pipeline is ready for data
  val pendingReg = Util.latchSync(audioPipeline.io.pcmData.ready && !io.mem.waitReq, io.mem.valid)
  val memRead = audioPipeline.io.pcmData.ready && !pendingReg
  val memAddr = channelStateReg.addr

  // Clear accumulator
  when(stateReg === State.idle) { accumulatorReg := Audio.zero(config.internalSampleWidth) }

  // Start/stop/reset channel
  when(stateReg === State.check) {
    when(start) {
      channelStateReg.start(channelReg.startAddr)
    }.elsewhen(stop) {
      channelStateReg.stop()
    }.elsewhen(zero) {
      channelStateReg.deactivate()
    }
  }

  // PCM data has been fetched
  when(audioPipeline.io.pcmData.fire()) { channelStateReg.nextAddr(channelReg) }

  // Audio pipeline has produced valid output
  when(audioPipeline.io.out.valid) {
    // Sum pipeline audio output with the accumulator
    accumulatorReg := accumulatorReg + audioPipeline.io.out.bits.audio

    // Update pipeline state
    channelStateReg.audioPipelineState := audioPipeline.io.out.bits.state
  }

  // Write channel state to memory
  when(stateReg === State.init || stateReg === State.write) {
    val data = Mux(stateReg === State.write, channelStateReg, ChannelState.default(config))
    channelStateMem.write(channelCounter, data.asUInt)
  }

  // FSM
  switch(stateReg) {
    // Initialize channel states
    is(State.init) {
      when(channelCounterWrap) { stateReg := State.idle }
    }

    // Clear accumulator
    is(State.idle) {
      when(io.enable) { stateReg := State.read }
    }

    // Read channel state from memory
    is(State.read) { stateReg := State.latch }

    // Latch channel state
    is(State.latch) { stateReg := State.check }

    // Check whether the channel is active
    is(State.check) { stateReg := Mux(active, State.ready, State.write) }

    // Wait for the pipeline to be ready
    is(State.ready) {
      when(audioPipeline.io.in.ready) { stateReg := State.process }
    }

    // Process channel through audio pipeline
    is(State.process) {
      when(audioPipeline.io.out.valid) { stateReg := State.write }
    }

    // Write channel state to memory
    is(State.write) { stateReg := State.next }

    // Increment channel index
    is(State.next) { stateReg := Mux(channelCounterWrap, State.done, State.read) }

    // All channels processed, write audio output
    is(State.done) {
      when(outputCounterWrap) { stateReg := State.idle }
    }
  }

  // Outputs
  io.index := channelCounter
  io.active := stateReg === State.check && active
  io.done := stateReg === State.check && done
  io.audio.valid := outputCounterWrap
  io.audio.bits := accumulatorReg.clamp(-32768, 32767)
  io.mem.rd := memRead
  io.mem.addr := memAddr
  io.debug.init := stateReg === State.init
  io.debug.idle := stateReg === State.idle
  io.debug.read := stateReg === State.read
  io.debug.latch := stateReg === State.latch
  io.debug.check := stateReg === State.check
  io.debug.ready := stateReg === State.ready
  io.debug.process := stateReg === State.process
  io.debug.write := stateReg === State.write
  io.debug.next := stateReg === State.next
  io.debug.done := stateReg === State.done
  io.debug.channelReg := channelStateReg

  printf(p"ChannelController(state: $stateReg, index: $channelCounter ($channelCounterWrap), channelState: $channelStateReg, audio: $accumulatorReg ($outputCounterWrap))\n")
}
