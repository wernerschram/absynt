package assembler.Elf

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
  val encodeBytes: List[Byte] = id :: version :: List.fill(7)(0x00.toByte)
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

  def flagBytes(flags: Flags[_])(implicit  endianness: Endianness): List[Byte]
  def numberBytes(number: Long)(implicit  endianness: Endianness): List[Byte]
  def programHeaderOffsetBytes(implicit endianness: Endianness): List[Byte]
  def sectionHeaderOffsetBytes(headerCount: Int)(implicit endianness: Endianness): List[Byte]
}
case object ProcessorClass {

  object _32Bit extends ProcessorClass(0x01.toByte) {
    override val headerSize: Short = 0x34
    override val programHeaderSize: Short = 0x20
    override val sectionHeaderSize: Short = 0x28

    override def flagBytes(flags: Flags[_])(implicit  endianness: Endianness): List[Byte] = endianness.encode((flags.encode).toInt)
    override def numberBytes(number: Long)(implicit  endianness: Endianness): List[Byte] = endianness.encode(number.toInt)
    override def programHeaderOffsetBytes(implicit endianness: Endianness): List[Byte] = endianness.encode(headerSize.toInt)
    override def sectionHeaderOffsetBytes(headerCount: Int)(implicit endianness: Endianness): List[Byte] =
      endianness.encode(headerSize + headerCount * programHeaderSize)
  }
  object _64Bit extends ProcessorClass(0x02.toByte) {
    override val headerSize: Short = 0x40
    override val programHeaderSize: Short = 0x38
    override val sectionHeaderSize: Short = 0x40

    override def flagBytes(flags: Flags[_])(implicit  endianness: Endianness): List[Byte] = endianness.encode(flags.encode)
    override def numberBytes(Number: Long)(implicit  endianness: Endianness): List[Byte] = endianness.encode(Number)
    override def programHeaderOffsetBytes(implicit endianness: Endianness): List[Byte] = endianness.encode(headerSize.toLong)
    override def sectionHeaderOffsetBytes(headerCount: Int)(implicit endianness: Endianness): List[Byte] =
      endianness.encode((headerSize + headerCount * programHeaderSize).toLong)
  }
}

abstract case class Endianness private(id: Byte) {
  def encode(value: Short): List[Byte]
  def encode(value: Int): List[Byte]
  def encode(value: Long): List[Byte]
}

case object Endianness {

  import assembler.ListExtensions._
 object LittleEndian extends Endianness(0x01.toByte) {
    override def encode(value: Short): List[Byte] = value.encodeLittleEndian
    override def encode(value: Int): List[Byte] = value.encodeLittleEndian
    override def encode(value: Long): List[Byte] = value.encodeLittleEndian
  }

  object BigEndian extends Endianness(0x02.toByte) {
    override def encode(value: Short): List[Byte] = value.encodeBigEndian
    override def encode(value: Int): List[Byte] = value.encodeBigEndian
    override def encode(value: Long): List[Byte] = value.encodeBigEndian
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
