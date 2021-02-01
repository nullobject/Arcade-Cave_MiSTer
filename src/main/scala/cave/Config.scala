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

package cave

import axon.gfx.VideoTimingConfig
import axon.mem.{DDRConfig, SDRAMConfig}
import axon.snd.YMZ280BConfig
import chisel3.util.log2Ceil

object Config {
  /**
   * System clock frequency (Hz)
   *
   * This is the clock domain used by the memory, GPU, etc.
   */
  val CLOCK_FREQ = 96000000D

  /**
   * CPU clock frequency (Hz)
   *
   * The CPU clock as measured on the PCB is 16MHz, but the M68k CPU module requires a clock that is
   * twice the desired clock speed.
   */
  val CPU_CLOCK_FREQ = 32000000D
  /** CPU clock period (ns) */
  val CPU_CLOCK_PERIOD = 1 / CPU_CLOCK_FREQ * 1000000000

  /**
   * Video clock frequency (Hz)
   *
   * The video clock as measured on the PCB is 28MHz, which is divided by 4 to give a 7MHz pixel
   * clock.
   */
  val VIDEO_CLOCK_FREQ = 28000000D
  /** Video clock divider */
  val VIDEO_CLOCK_DIV = 4

  /** The screen width in pixels */
  val SCREEN_WIDTH = 320
  /** The screen height in pixels */
  val SCREEN_HEIGHT = 240

  /** YMZ280B configuration */
  val ymzConfig = YMZ280BConfig(
    clockFreq = CPU_CLOCK_FREQ,
    sampleFreq = 88200
  )

  /** DDR configuration */
  val ddrConfig = DDRConfig()

  /** SDRAM configuration */
  val sdramConfig = SDRAMConfig(
    clockFreq = CLOCK_FREQ,
    burstLength = 4
  )

  /** Original (57Hz) video timing configuration */
  val originalVideoTimingConfig = VideoTimingConfig(
    clockFreq = VIDEO_CLOCK_FREQ,
    clockDiv = VIDEO_CLOCK_DIV,
    hFreq = 15625,
    vFreq = 57.44,
    hDisplay = SCREEN_WIDTH,
    vDisplay = SCREEN_HEIGHT,
    hFrontPorch = 30,
    vFrontPorch = 12,
    hRetrace = 20,
    vRetrace = 2,
  )

  /** Compatibility (60Hz) video timing configuration */
  val compatibilityVideoTimingConfig = VideoTimingConfig(
    clockFreq = VIDEO_CLOCK_FREQ,
    clockDiv = VIDEO_CLOCK_DIV,
    hFreq = 15625,
    vFreq = 60,
    hDisplay = SCREEN_WIDTH,
    vDisplay = SCREEN_HEIGHT,
    hFrontPorch = 30,
    vFrontPorch = 12,
    hRetrace = 20,
    vRetrace = 2,
  )

  /** The width of the pulse generated when a player presses the coin button */
  val PLAYER_COIN_PULSE_WIDTH = (100000000 / CPU_CLOCK_PERIOD).ceil.toInt // 100ms

  val FRAME_BUFFER_OFFSET = 0x24000000
  val DDR_DOWNLOAD_OFFSET = 0x30000000

  val PROG_ROM_ADDR_WIDTH = 20 // 1MB
  val PROG_ROM_DATA_WIDTH = 16

  val SOUND_ROM_ADDR_WIDTH = 25
  val SOUND_ROM_DATA_WIDTH = 8

  val TILE_ROM_ADDR_WIDTH = 32
  val TILE_ROM_DATA_WIDTH = 64

  val MAIN_RAM_ADDR_WIDTH = 15
  val MAIN_RAM_DATA_WIDTH = 16

  val SPRITE_RAM_ADDR_WIDTH = 15
  val SPRITE_RAM_DATA_WIDTH = 16
  val SPRITE_RAM_GPU_ADDR_WIDTH = 12
  val SPRITE_RAM_GPU_DATA_WIDTH = 128

  val LAYER_RAM_ADDR_WIDTH = 14
  val LAYER_RAM_DATA_WIDTH = 16
  val LAYER_RAM_GPU_ADDR_WIDTH = 13
  val LAYER_RAM_GPU_DATA_WIDTH = 32

  val PALETTE_RAM_ADDR_WIDTH = 15
  val PALETTE_RAM_DATA_WIDTH = 16
  val PALETTE_RAM_GPU_ADDR_WIDTH = 15
  val PALETTE_RAM_GPU_DATA_WIDTH = 16

  /** The number of bits per color channel */
  val BITS_PER_CHANNEL = 5

  val LAYER_REGS_COUNT = 3
  val LAYER_REGS_GPU_DATA_WIDTH = 48

  val VIDEO_REGS_COUNT = 8
  val VIDEO_REGS_GPU_DATA_WIDTH = 128

  /** The number of bits per color channel for the DDR frame buffer */
  val DDR_FRAME_BUFFER_BITS_PER_CHANNEL = 8
  /** The bit depth of a DDR frame buffer pixel */
  val DDR_FRAME_BUFFER_BPP = 32

  /** The depth of the frame buffer in words */
  val FRAME_BUFFER_DEPTH = SCREEN_WIDTH * SCREEN_HEIGHT
  /** The width of the frame buffer X address */
  val FRAME_BUFFER_ADDR_WIDTH_X = log2Ceil(SCREEN_WIDTH)
  /** The width of the frame buffer Y address */
  val FRAME_BUFFER_ADDR_WIDTH_Y = log2Ceil(SCREEN_HEIGHT)
  /** The width of the frame buffer address bus */
  val FRAME_BUFFER_ADDR_WIDTH = log2Ceil(FRAME_BUFFER_DEPTH)
  /** The width of the frame buffer data bus */
  val FRAME_BUFFER_DATA_WIDTH = 15

  /** The bit depth of a frame buffer DMA pixel */
  val FRAME_BUFFER_DMA_BPP = 24
  /** The number of pixels transferred per word during frame buffer DMA */
  val FRAME_BUFFER_DMA_PIXELS = 2
  /** The depth of the frame buffer DMA in words */
  val FRAME_BUFFER_DMA_DEPTH = SCREEN_WIDTH * SCREEN_HEIGHT / FRAME_BUFFER_DMA_PIXELS
  /** The width of the frame buffer DMA address bus */
  val FRAME_BUFFER_DMA_ADDR_WIDTH = log2Ceil(FRAME_BUFFER_DMA_DEPTH)
  /** The width of the frame buffer DMA data bus */
  val FRAME_BUFFER_DMA_DATA_WIDTH = FRAME_BUFFER_DMA_BPP * FRAME_BUFFER_DMA_PIXELS
  /** The number of words to transfer during frame buffer DMA */
  val FRAME_BUFFER_DMA_NUM_WORDS = SCREEN_WIDTH * SCREEN_HEIGHT * DDR_FRAME_BUFFER_BPP / ddrConfig.dataWidth
  /** The length of a burst during a frame buffer DMA transfer */
  val FRAME_BUFFER_DMA_BURST_LENGTH = 128

  /** The width of the priority buffer address bus */
  val PRIO_BUFFER_ADDR_WIDTH = log2Ceil(FRAME_BUFFER_DEPTH)
  /** The width of the priority buffer data bus */
  val PRIO_BUFFER_DATA_WIDTH = 2

  /** The width of a priority value */
  val PRIO_WIDTH = 2
  /** The width of a color code value */
  val COLOR_CODE_WIDTH = 6
  /** The width of the layer index value */
  val LAYER_INDEX_WIDTH = 2
  /** The width of the layer scroll value */
  val LAYER_SCROLL_WIDTH = 9

  /** The bit depth of a small tile pixel */
  val SMALL_TILE_BPP = 8
  /** The size of a small tile in pixels */
  val SMALL_TILE_SIZE = 8
  /** The size of a large tile in bytes */
  val SMALL_TILE_BYTE_SIZE = SMALL_TILE_SIZE * 8
  /** The number of small tiles that fit horizontally on the screen */
  val SMALL_TILE_NUM_COLS = SCREEN_WIDTH / SMALL_TILE_SIZE + 1
  /** The number of small tiles that fit vertically on the screen */
  val SMALL_TILE_NUM_ROWS = SCREEN_HEIGHT / SMALL_TILE_SIZE + 1
  /** The number of small tiles that fit on the screen */
  val SMALL_TILE_NUM_TILES = SMALL_TILE_NUM_COLS * SMALL_TILE_NUM_ROWS

  /** The bit depth of a large tile pixel */
  val LARGE_TILE_BPP = 4
  /** The size of a large tile in pixels */
  val LARGE_TILE_SIZE = 16
  /** The size of a large tile in bytes */
  val LARGE_TILE_BYTE_SIZE = LARGE_TILE_SIZE * 8
  /** The number of large tiles that fit horizontally on the screen */
  val LARGE_TILE_NUM_COLS = SCREEN_WIDTH / LARGE_TILE_SIZE + 1
  /** The number of large tiles that fit vertically on the screen */
  val LARGE_TILE_NUM_ROWS = SCREEN_HEIGHT / LARGE_TILE_SIZE + 1
  /** The number of large tiles that fit on the screen */
  val LARGE_TILE_NUM_TILES = LARGE_TILE_NUM_COLS * LARGE_TILE_NUM_ROWS
}
