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

/** Represents the state of an audio pipeline. */
class AudioPipelineState(private val config: YMZ280BConfig) extends Bundle {
  /** Sample values */
  val samples = Vec(2, SInt(config.sampleWidth.W))
  /** Asserted when a new sample is required */
  val underflow = Bool()
  /** ADPCM step value */
  val adpcmStep = SInt(config.sampleWidth.W)
  /** Interpolation index */
  val lerpIndex = UInt(config.lerpIndexWidth.W)
  /** Asserted when the loop step and sample values are cached */
  val loopEnable = Bool()
  /** Cached loop ADPCM step value */
  val loopStep = SInt(config.sampleWidth.W)
  /** Cached loop sample value */
  val loopSample = SInt(config.sampleWidth.W)

  /** Updates the ADPCM state with the given step and sample values. */
  def adpcm(step: SInt, sample: SInt) = {
    adpcmStep := step
    samples := VecInit(samples(1), sample)
  }

  /** Updates the interpolation index with the given pitch value. */
  def interpolate(pitch: UInt) = {
    val index = lerpIndex + pitch + 1.U
    underflow := index.head(1)
    lerpIndex := index.tail(1)
  }

  /** Asserted when the current sample is zero. */
  def isZero = samples(1) === 0.S
}

object AudioPipelineState {
  /** Returns the default audio pipeline state. */
  def default(config: YMZ280BConfig): AudioPipelineState = {
    val state = Wire(new AudioPipelineState(config))
    state.samples := VecInit(0.S, 0.S)
    state.adpcmStep := 127.S
    state.lerpIndex := 0.U
    state.underflow := true.B
    state.loopEnable := false.B
    state.loopStep := 0.S
    state.loopSample := 0.S
    state
  }
}
