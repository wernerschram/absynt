/*
 * Copyright 2019 Werner Schram
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use
 * this file except in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */

package org.werner.absynt.output.Elf

trait Architecture {
  def endianness: Endianness
  def ABI: OSABI
  def processor: Processor
  def processorClass: ProcessorClass
}

object Architecture {
  object RaspberryPi2 extends Architecture {
    override val endianness: Endianness = Endianness.LittleEndian
    override val ABI: OSABI = OSABI.SystemV
    override val processor: Processor = Processor.ARM
    override val processorClass: ProcessorClass = ProcessorClass._32Bit
  }

  object X86 extends Architecture {
    override val endianness: Endianness = Endianness.LittleEndian
    override val ABI: OSABI = OSABI.SystemV
    override val processor: Processor = Processor.X86
    override val processorClass: ProcessorClass = ProcessorClass._32Bit
  }

  object X86_64 extends Architecture {
    override val endianness: Endianness = Endianness.LittleEndian
    override val ABI: OSABI = OSABI.SystemV
    override val processor: Processor = Processor.X86_64
    override val processorClass: ProcessorClass = ProcessorClass._64Bit
  }
}

abstract case class OSABI private(id: Byte, version: Byte) {
  val encodeBytes: Seq[Byte] = id +: version +: Seq.fill(7)(0x00.toByte)
}

case object OSABI {
  object SystemV extends OSABI(0x00.toByte, 0x00.toByte)
  object HPUX extends OSABI(0x01.toByte, 0x00.toByte)
  object NetBSD extends OSABI(0x02.toByte, 0x00.toByte)
  object Linux extends OSABI(0x03.toByte, 0x00.toByte)
  //...
}

abstract sealed case class ProcessorClass private(id: Byte) {
  def headerSize: Short
  def programHeaderSize: Short
  def sectionHeaderSize: Short
  def numberSize: Byte

  def flagBytes(flags: Flags[?])(implicit  endianness: Endianness): Seq[Byte]
  def numberBytes(number: Long)(implicit  endianness: Endianness): Seq[Byte]
  def programHeaderOffsetBytes(implicit endianness: Endianness): Seq[Byte]
  def sectionHeaderOffsetBytes(headerCount: Int)(implicit endianness: Endianness): Seq[Byte]
}
case object ProcessorClass {

  object _32Bit extends ProcessorClass(0x01.toByte) {
    override val headerSize: Short = 0x34
    override val programHeaderSize: Short = 0x20
    override val sectionHeaderSize: Short = 0x28
    override val numberSize: Byte = 4

    override def flagBytes(flags: Flags[?])(implicit  endianness: Endianness): Seq[Byte] = endianness.encode(flags.encode.toInt)
    override def numberBytes(number: Long)(implicit  endianness: Endianness): Seq[Byte] = endianness.encode(number.toInt)
    override def programHeaderOffsetBytes(implicit endianness: Endianness): Seq[Byte] = endianness.encode(headerSize.toInt)
    override def sectionHeaderOffsetBytes(headerCount: Int)(implicit endianness: Endianness): Seq[Byte] =
      endianness.encode(headerSize + headerCount * programHeaderSize)
  }
  object _64Bit extends ProcessorClass(0x02.toByte) {
    override val headerSize: Short = 0x40
    override val programHeaderSize: Short = 0x38
    override val sectionHeaderSize: Short = 0x40
    override val numberSize: Byte = 8

    override def flagBytes(flags: Flags[?])(implicit  endianness: Endianness): Seq[Byte] = endianness.encode(flags.encode)
    override def numberBytes(number: Long)(implicit  endianness: Endianness): Seq[Byte] = endianness.encode(number)
    override def programHeaderOffsetBytes(implicit endianness: Endianness): Seq[Byte] = endianness.encode(headerSize.toLong)
    override def sectionHeaderOffsetBytes(headerCount: Int)(implicit endianness: Endianness): Seq[Byte] =
      endianness.encode((headerSize + headerCount * programHeaderSize).toLong)
  }
}

abstract case class Endianness private(id: Byte) {
  def encode(value: Short): Seq[Byte]
  def encode(value: Int): Seq[Byte]
  def encode(value: Long): Seq[Byte]
}

case object Endianness {

  import org.werner.absynt.ListExtensions._
  object LittleEndian extends Endianness(0x01.toByte) {
    override def encode(value: Short): Seq[Byte] = value.encodeLittleEndian
    override def encode(value: Int): Seq[Byte] = value.encodeLittleEndian
    override def encode(value: Long): Seq[Byte] = value.encodeLittleEndian

    override def toString: String = "LittleEndian"
  }

  object BigEndian extends Endianness(0x02.toByte) {
    override def encode(value: Short): Seq[Byte] = value.encodeBigEndian
    override def encode(value: Int): Seq[Byte] = value.encodeBigEndian
    override def encode(value: Long): Seq[Byte] = value.encodeBigEndian

    override def toString: String = "BigEndian"
  }
}

case class Processor private(id: Short, flags: Int)

case object Processor {
  object Undefined extends Processor(0x00.toShort, 0x00)
  object SPARC extends Processor(0x02.toShort, 0x00)
  object X86 extends Processor(0x03.toShort, 0x00)
  object MIPS extends Processor(0x08.toShort, 0x00)
  object PowerPC extends Processor(0x14.toShort, 0x00)
  //...
  object ARM extends Processor(0x28.toShort, 0x00)
  object X86_64 extends Processor(0x3e.toShort, 0x00)
}
