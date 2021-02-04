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
import axon.mister._
import axon.types._
import cave.types._
import chisel3._

/**
 * The memory subsystem routes memory requests to either DDR or SDRAM.
 *
 * The program and sound ROMs are stored in SDRAM, because they require low-latency memory access.
 * ROM data is also cached to reduce the need to go to memory for every read request.
 *
 * Everything else is stored in DDR memory.
 */
class MemSys extends Module {
  val io = IO(new Bundle {
    /** Game config port */
    val gameConfig = Input(GameConfig())
    /** Options port */
    val options = OptionsIO()
    /** IOCTL port */
    val ioctl = IOCTL()
    /** Program ROM port */
    val progRom = Flipped(new ProgRomIO)
    /** Sound ROM port */
    val soundRom = Flipped(new SoundRomIO)
    /** EEPROM port */
    val eeprom = Flipped(new EEPROMIO)
    /** Tile ROM port */
    val tileRom = Flipped(new TileRomIO)
    /** Frame buffer DMA port */
    val frameBufferDMA = Flipped(BurstWriteMemIO(Config.ddrConfig.addrWidth, Config.ddrConfig.dataWidth))
    /** Video DMA port */
    val videoDMA = Flipped(BurstReadMemIO(Config.ddrConfig.addrWidth, Config.ddrConfig.dataWidth))
    /** DDR port */
    val ddr = BurstReadWriteMemIO(Config.ddrConfig.addrWidth, Config.ddrConfig.dataWidth)
    /** SDRAM port */
    val sdram = BurstReadWriteMemIO(Config.sdramConfig.addrWidth, Config.sdramConfig.dataWidth)
  })

  // The DDR download cache is used to buffer download data, so that a complete word can be written
  // to memory.
  val ddrDownloadCache = Module(new Cache(CacheConfig(
    inAddrWidth = IOCTL.ADDR_WIDTH,
    inDataWidth = IOCTL.DATA_WIDTH,
    outAddrWidth = Config.ddrConfig.addrWidth,
    outDataWidth = Config.ddrConfig.dataWidth,
    lineWidth = 1,
    depth = 1
  )))
  ddrDownloadCache.io.offset := Config.DDR_DOWNLOAD_OFFSET.U
  ddrDownloadCache.io.in <> io.ioctl.asAsyncReadWriteMemIO(IOCTL.ROM_INDEX)

  // The SDRAM download cache is used to buffer download data, so that a complete word can be
  // written to memory.
  val sdramDownloadCache = Module(new Cache(CacheConfig(
    inAddrWidth = IOCTL.ADDR_WIDTH,
    inDataWidth = IOCTL.DATA_WIDTH,
    outAddrWidth = Config.sdramConfig.addrWidth,
    outDataWidth = Config.sdramConfig.dataWidth,
    lineWidth = Config.sdramConfig.burstLength,
    depth = 1,
    wrapping = true
  )))
  sdramDownloadCache.io.offset := 0.U
  sdramDownloadCache.io.in <> io.ioctl.asAsyncReadWriteMemIO(IOCTL.ROM_INDEX)

  // Program ROM cache
  val progRomCache1 = Module(new Cache(CacheConfig(
    inAddrWidth = Config.PROG_ROM_ADDR_WIDTH,
    inDataWidth = Config.PROG_ROM_DATA_WIDTH,
    outAddrWidth = Config.ddrConfig.addrWidth,
    outDataWidth = Config.ddrConfig.dataWidth,
    lineWidth = 4,
    depth = 256
  )))
  progRomCache1.io.offset := io.gameConfig.progRomOffset + Config.DDR_DOWNLOAD_OFFSET.U
  val progRomCache2 = Module(new Cache(CacheConfig(
    inAddrWidth = Config.PROG_ROM_ADDR_WIDTH,
    inDataWidth = Config.PROG_ROM_DATA_WIDTH,
    outAddrWidth = Config.sdramConfig.addrWidth,
    outDataWidth = Config.sdramConfig.dataWidth,
    lineWidth = Config.sdramConfig.burstLength,
    depth = 256,
    wrapping = true
  )))
  progRomCache2.io.offset := io.gameConfig.progRomOffset

  // Sound ROM cache
  val soundRomCache1 = Module(new Cache(CacheConfig(
    inAddrWidth = Config.SOUND_ROM_ADDR_WIDTH,
    inDataWidth = Config.SOUND_ROM_DATA_WIDTH,
    outAddrWidth = Config.ddrConfig.addrWidth,
    outDataWidth = Config.ddrConfig.dataWidth,
    lineWidth = 4,
    depth = 256,
    wrapping = true
  )))
  soundRomCache1.io.offset := io.gameConfig.soundRomOffset + Config.DDR_DOWNLOAD_OFFSET.U
  val soundRomCache2 = Module(new Cache(CacheConfig(
    inAddrWidth = Config.SOUND_ROM_ADDR_WIDTH,
    inDataWidth = Config.SOUND_ROM_DATA_WIDTH,
    outAddrWidth = Config.sdramConfig.addrWidth,
    outDataWidth = Config.sdramConfig.dataWidth,
    lineWidth = Config.sdramConfig.burstLength,
    depth = 256,
    wrapping = true
  )))
  soundRomCache2.io.offset := io.gameConfig.soundRomOffset

  // EEPROM cache
  val eepromCache = Module(new Cache(CacheConfig(
    inAddrWidth = Config.EEPROM_ADDR_WIDTH,
    inDataWidth = Config.EEPROM_DATA_WIDTH,
    outAddrWidth = Config.ddrConfig.addrWidth,
    outDataWidth = Config.ddrConfig.dataWidth,
    lineWidth = 4,
    depth = 4
  )))
  eepromCache.io.offset := io.gameConfig.eepromOffset + Config.DDR_DOWNLOAD_OFFSET.U

  // DDR arbiter
  val ddrArbiter = Module(new MemArbiter(7, Config.ddrConfig.addrWidth, Config.ddrConfig.dataWidth))
  ddrArbiter.io.in(0) <> ddrDownloadCache.io.out
  ddrArbiter.io.in(1) <> progRomCache1.io.out
  ddrArbiter.io.in(2) <> soundRomCache1.io.out
  ddrArbiter.io.in(3) <> eepromCache.io.out
  ddrArbiter.io.in(4).asBurstReadMemIO <> io.videoDMA
  ddrArbiter.io.in(5).asBurstWriteMemIO <> io.frameBufferDMA
  ddrArbiter.io.in(6).asBurstReadMemIO <> io.tileRom
  ddrArbiter.io.in(6).addr := io.tileRom.addr + Config.DDR_DOWNLOAD_OFFSET.U // override tile ROM address
  ddrArbiter.io.out <> io.ddr

  // SDRAM arbiter
  val sdramArbiter = Module(new MemArbiter(3, Config.sdramConfig.addrWidth, Config.sdramConfig.dataWidth))
  sdramArbiter.io.in(0) <> sdramDownloadCache.io.out
  sdramArbiter.io.in(1) <> soundRomCache2.io.out
  sdramArbiter.io.in(2) <> progRomCache2.io.out
  sdramArbiter.io.out <> io.sdram

  io.progRom <> AsyncReadWriteMemIO.demux(io.options.sdram, Seq(
    false.B -> progRomCache1.io.in,
    true.B -> progRomCache2.io.in
  )).asAsyncReadMemIO

  io.soundRom <> AsyncReadWriteMemIO.demux(io.options.sdram, Seq(
    false.B -> soundRomCache1.io.in,
    true.B -> soundRomCache2.io.in
  )).asAsyncReadMemIO

  io.eeprom <> eepromCache.io.in

  // Wait until both DDR and SDRAM are ready
  io.ioctl.waitReq := ddrDownloadCache.io.in.waitReq || (io.options.sdram && sdramDownloadCache.io.in.waitReq)
}
