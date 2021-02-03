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

package axon.mem

import chisel3._
import chisel3.util._

abstract class MemIO protected(val addrWidth: Int, val dataWidth: Int) extends Bundle {
  def maskWidth = dataWidth / 8
}

/**
 * A simple flow control interface for reading from synchronous memory.
 *
 * @param addrWidth The width of the address bus.
 * @param dataWidth The width of the data bus.
 */
class ReadMemIO protected(addrWidth: Int, dataWidth: Int) extends MemIO(addrWidth, dataWidth) {
  /** Read enable */
  val rd = Output(Bool())
  /** Address bus */
  val addr = Output(UInt(addrWidth.W))
  /** Data bus */
  val dout = Input(UInt(dataWidth.W))

  override def cloneType: this.type = new ReadMemIO(addrWidth, dataWidth).asInstanceOf[this.type]

  /**
   * Maps the address using the given function.
   *
   * @param f The transform function.
   */
  def mapAddr(f: UInt => UInt): ReadMemIO = {
    val mem = Wire(chiselTypeOf(this))
    mem.rd := this.rd
    mem.addr := f(this.addr)
    this.dout := mem.dout
    mem
  }

  def default() = {
    rd := false.B
    addr := DontCare
  }
}

object ReadMemIO {
  def apply(addrWidth: Int, dataWidth: Int): ReadMemIO = new ReadMemIO(addrWidth, dataWidth)

  /**
   * Multiplexes requests from two read-only memory interfaces to a single read-only memory
   * interface.
   *
   * @param select Selects between the two memory interfaces.
   * @param a      The first interface.
   * @param b      The second interface.
   */
  def mux(select: Bool, a: ReadMemIO, b: ReadMemIO): ReadMemIO = {
    val mem = Wire(chiselTypeOf(a))
    mem.rd := Mux(select, a.rd, b.rd)
    mem.addr := Mux(select, a.addr, b.addr)
    a.dout := mem.dout
    b.dout := mem.dout
    mem
  }

  /**
   * Multiplexes requests from read-only memory interfaces to a single read-only memory
   * interface.
   *
   * @param key     The key to used to select the interface.
   * @param default The default interface.
   * @param mapping A list of key-interface pairs.
   */
  def muxLookup[K <: UInt](key: K, default: ReadMemIO, mapping: Seq[(K, ReadMemIO)]): ReadMemIO = {
    val mem = Wire(chiselTypeOf(mapping.head._2))
    val rdMap = mapping.map(a => a._1 -> a._2.rd)
    val addrMap = mapping.map(a => a._1 -> a._2.addr)
    mem.rd := MuxLookup(key, default.rd, rdMap)
    mem.addr := MuxLookup(key, default.addr, addrMap)
    default.dout := mem.dout
    mapping.foreach { case (_, out) =>
      out.dout := mem.dout
    }
    mem
  }

  /**
   * Demultiplexes requests from a single read-only memory interface to multiple read-only memory
   * interfaces. The request is routed to the interface matching a given key.
   *
   * @param key  The key to used to select the interface.
   * @param outs A list of key-interface pairs.
   */
  def demux[K <: UInt](key: K, outs: Seq[(K, ReadMemIO)]): ReadMemIO = {
    val mem = Wire(chiselTypeOf(outs.head._2))
    outs.foreach { case (k, out) =>
      out.rd := k === key && mem.rd
      out.addr := mem.addr
    }
    val doutMap = outs.map(a => a._1 -> a._2.dout)
    mem.dout := MuxLookup(key, 0.U, doutMap)
    mem
  }
}

/**
 * A simple flow control interface for writing to synchronous memory.
 *
 * @param addrWidth The width of the address bus.
 * @param dataWidth The width of the data bus.
 */
class WriteMemIO protected(addrWidth: Int, dataWidth: Int) extends MemIO(addrWidth, dataWidth) {
  /** Write enable */
  val wr = Output(Bool())
  /** Address bus */
  val addr = Output(UInt(addrWidth.W))
  /** Byte mask */
  val mask = Output(Bits(maskWidth.W))
  /** Data bus */
  val din = Output(UInt(dataWidth.W))

  override def cloneType: this.type = new WriteMemIO(addrWidth, dataWidth).asInstanceOf[this.type]

  /**
   * Maps the address using the given function.
   *
   * @param f The transform function.
   */
  def mapAddr(f: UInt => UInt): WriteMemIO = {
    val mem = Wire(chiselTypeOf(this))
    mem.wr := this.wr
    mem.addr := f(this.addr)
    mem.mask := this.mask
    mem.din := this.din
    mem
  }

  def default() = {
    wr := false.B
    addr := DontCare
    mask := DontCare
    din := DontCare
  }
}

object WriteMemIO {
  def apply(addrWidth: Int, dataWidth: Int): WriteMemIO = new WriteMemIO(addrWidth, dataWidth)

  /**
   * Multiplexes requests from multiple write-only memory interface to a single write-only memory interfaces. The
   * request is routed to the first enabled interface.
   *
   * @param outs A list of enable-interface pairs.
   */
  def mux[K <: UInt](key: K, outs: Seq[(K, WriteMemIO)]): WriteMemIO = {
    val mem = Wire(chiselTypeOf(outs.head._2))
    val writeMap = outs.map(a => a._1 -> a._2.wr)
    val addrMap = outs.map(a => a._1 -> a._2.addr)
    val maskMap = outs.map(a => a._1 -> a._2.mask)
    val dataMap = outs.map(a => a._1 -> a._2.din)
    mem.wr := MuxLookup(key, false.B, writeMap)
    mem.addr := MuxLookup(key, DontCare, addrMap)
    mem.mask := MuxLookup(key, DontCare, maskMap)
    mem.din := MuxLookup(key, DontCare, dataMap)
    mem
  }
}

/**
 * A simple flow control interface for reading and writing to synchronous memory.
 *
 * @param addrWidth The width of the address bus.
 * @param dataWidth The width of the data bus.
 */
class ReadWriteMemIO protected(addrWidth: Int, dataWidth: Int) extends MemIO(addrWidth, dataWidth) {
  /** Read enable */
  val rd = Output(Bool())
  /** Write enable */
  val wr = Output(Bool())
  /** Address bus */
  val addr = Output(UInt(addrWidth.W))
  /** Byte mask */
  val mask = Output(Bits(maskWidth.W))
  /** Data input bus */
  val din = Output(Bits(dataWidth.W))
  /** Data output bus */
  val dout = Input(Bits(dataWidth.W))

  override def cloneType: this.type = new ReadWriteMemIO(addrWidth, dataWidth).asInstanceOf[this.type]

  /** Converts the interface to read-only */
  def asReadMemIO: ReadMemIO = {
    val mem = Wire(Flipped(ReadMemIO(addrWidth, dataWidth)))
    rd := mem.rd
    wr := false.B
    addr := mem.addr
    mask := DontCare
    din := DontCare
    mem.dout := dout
    mem
  }

  /** Converts the interface to write-only */
  def asWriteMemIO: WriteMemIO = {
    val mem = Wire(Flipped(WriteMemIO(addrWidth, dataWidth)))
    rd := false.B
    wr := mem.wr
    addr := mem.addr
    mask := mem.mask
    din := mem.din
    mem
  }

  def default() = {
    rd := false.B
    wr := false.B
    addr := DontCare
    mask := DontCare
    din := DontCare
  }
}

object ReadWriteMemIO {
  def apply(addrWidth: Int, dataWidth: Int): ReadWriteMemIO = new ReadWriteMemIO(addrWidth, dataWidth)
}
