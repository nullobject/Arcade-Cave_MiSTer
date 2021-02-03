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

import axon.mem._
import chisel3._

package object types {
  /** Frame buffer DMA IO */
  class FrameBufferDMAIO extends ReadMemIO(Config.FRAME_BUFFER_DMA_ADDR_WIDTH, Config.FRAME_BUFFER_DMA_DATA_WIDTH) {
    override def cloneType: this.type = new FrameBufferDMAIO().asInstanceOf[this.type]
  }

  /** Priority IO */
  class PriorityIO extends Bundle {
    /** Write-only port */
    val write = WriteMemIO(Config.PRIO_BUFFER_ADDR_WIDTH, Config.PRIO_BUFFER_DATA_WIDTH)
    /** Read-only port */
    val read = ReadMemIO(Config.PRIO_BUFFER_ADDR_WIDTH, Config.PRIO_BUFFER_DATA_WIDTH)
  }

  /** Program ROM IO */
  class ProgRomIO extends AsyncReadMemIO(Config.PROG_ROM_ADDR_WIDTH, Config.PROG_ROM_DATA_WIDTH) {
    override def cloneType: this.type = new ProgRomIO().asInstanceOf[this.type]
  }

  /** Sound ROM IO */
  class SoundRomIO extends AsyncReadMemIO(Config.SOUND_ROM_ADDR_WIDTH, Config.SOUND_ROM_DATA_WIDTH) {
    override def cloneType: this.type = new SoundRomIO().asInstanceOf[this.type]
  }

  /** EEPROM IO */
  class EEPROMIO extends AsyncReadWriteMemIO(Config.EEPROM_ADDR_WIDTH, Config.EEPROM_DATA_WIDTH) {
    override def cloneType: this.type = new EEPROMIO().asInstanceOf[this.type]
  }

  /** Tile ROM IO */
  class TileRomIO extends BurstReadMemIO(Config.TILE_ROM_ADDR_WIDTH, Config.TILE_ROM_DATA_WIDTH) {
    override def cloneType: this.type = new TileRomIO().asInstanceOf[this.type]
  }
}
