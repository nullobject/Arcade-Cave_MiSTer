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

/** Represents the state of a channel. */
class ChannelState(private val config: YMZ280BConfig) extends Bundle {
  /** Asserted when the channel has a key on */
  val keyOn = Bool()
  /** Asserted when the channel is fading out */
  val fadeOut = Bool()
  /** Asserted when the channel is playing a sample */
  val active = Bool()
  /** Asserted when the channel has completed playing a sample (i.e. reached the end address) */
  val complete = Bool()
  /** Asserted when the channel is processing the high ADPCM nibble */
  val nibble = Bool()
  /** Sample address */
  val addr = UInt(config.memAddrWidth.W)
  /** Asserted when the channel is at the start of a loop */
  val loopStart = Bool()
  /** Audio pipeline state */
  val audioPipelineState = new AudioPipelineState(config)

  /** Starts the channel at the given address. */
  def start(startAddr: UInt) = {
    keyOn := true.B
    fadeOut := false.B
    active := true.B
    complete := false.B
    nibble := false.B
    addr := startAddr
    loopStart := false.B
    audioPipelineState := AudioPipelineState.default(config)
  }

  /** Stops the channel. */
  def stop() = {
    keyOn := false.B
    fadeOut := true.B
    active := true.B
    complete := false.B
  }

  /** Reset the channel. */
  def reset() = {
    keyOn := false.B
    fadeOut := false.B
    active := false.B
    complete := false.B
  }

  /**
   * Moves the channel to the next address.
   *
   * If the channel has reached the end address, then the done flag is asserted.
   *
   * @param channelReg The channel register.
   */
  def nextAddr(channelReg: ChannelReg) = {
    loopStart := channelReg.flags.loop && addr === channelReg.loopStartAddr && !nibble
    nibble := !nibble
    when(nibble) {
      when(channelReg.flags.loop && addr === channelReg.loopEndAddr) {
        addr := channelReg.loopStartAddr
      }.elsewhen(addr === channelReg.endAddr) {
        fadeOut := true.B
        complete := true.B
      }.otherwise {
        addr := addr + 1.U
      }
    }
  }
}

object ChannelState {
  /** Returns the default channel state. */
  def default(config: YMZ280BConfig): ChannelState = {
    val state = Wire(new ChannelState(config))
    state.keyOn := false.B
    state.fadeOut := false.B
    state.active := false.B
    state.complete := false.B
    state.nibble := false.B
    state.addr := 0.U
    state.loopStart := false.B
    state.audioPipelineState := AudioPipelineState.default(config)
    state
  }
}
