package assembler.Elf

import assembler._
import assembler.sections.Section

abstract class Elf[S <: Section:HasName](val architecture: Architecture, sections: List[S], val entryLabel: Label) extends Application(sections) {
  val magic: List[Byte] = 0x7F.toByte :: Nil ::: "ELF".toCharArray.map(_.toByte).toList
  val version: ElfVersion = ElfVersion.Original

  implicit def endianness: Endianness = architecture.endianness

  def elfType: ElfType

  val programHeaderCount: Short = sections.size.toShort
  val sectionHeaderCount: Short = (sections.size + 2).toShort // program sections + Null section and Strings section
  val sectionNamesSectionHeaderIndex: Short = (sectionHeaderCount - 1).toShort // last section

  val dataOffset: Int =
    architecture.processorClass.headerSize +
    programHeaderCount * architecture.processorClass.programHeaderSize +
    sectionHeaderCount * architecture.processorClass.sectionHeaderSize

  def address(label: Label): Long =
    sections.filter(s => s.contains(label)).map(s => s.baseAddress + s.relativeAddress(label)).head

  def fileOffset(section: Section): Long =
    dataOffset + sections.takeWhile(s => s != section).map(s => s.size).sum

  def stringTableOffset: Long =
    dataOffset + sections.map(s => s.size).sum

  def stringOffset(strings: List[String]): List[(String, Int)] = {
    (strings.head, 0) :: stringOffset(1, strings)
  }

  private def stringOffset(startOffset: Int, strings: List[String]): List[(String, Int)] = {
    strings match {
      case head :: neck :: Nil => (neck, startOffset + head.length) :: Nil
      case head :: neck :: tail => (neck, startOffset + head.length) :: stringOffset(startOffset + head.length + 1, neck :: tail)
      case neck :: Nil => (neck, startOffset) :: Nil
      case Nil => Nil
    }
  }

  val stringMap: Map[String, Int] =
    stringOffset(("" :: sections.map(s => implicitly[HasName[S]].name(s)) ::: ".shstrtab" :: Nil )
       .distinct).toMap

  implicit val executable: Elf[S] = this

  val programHeaders: List[ProgramHeader] =
    sections.map(s => ProgramHeader(s))
  val sectionHeaders: List[SectionHeader[S]] =
    NullSectionHeader[S]() ::
    sections.map(s => new SectionSectionHeader[S](s)) :::
    new StringSectionHeader[S]() :: Nil

  def header: List[Byte] =
      magic :::
      architecture.processorClass.id ::
      endianness.id ::
      version.id ::
      architecture.ABI.encodeBytes :::
      endianness.encode(elfType.id) :::
      endianness.encode(architecture.processor.id) :::
      endianness.encode(version.extended) :::
      architecture.processorClass.numberBytes(address(entryLabel)) :::
      architecture.processorClass.programHeaderOffsetBytes :::
      architecture.processorClass.sectionHeaderOffsetBytes(programHeaderCount) :::
      endianness.encode(architecture.processor.flags) :::
      endianness.encode(architecture.processorClass.headerSize) :::
      endianness.encode(architecture.processorClass.programHeaderSize) :::
      endianness.encode(programHeaderCount) :::
      endianness.encode(architecture.processorClass.sectionHeaderSize) :::
      endianness.encode(sectionHeaderCount) :::
      endianness.encode(sectionNamesSectionHeaderIndex) :::
      programHeaders.flatMap(p => p.header) :::
      sectionHeaders.flatMap(s => s.header) :::
      sections.flatMap(s => s.encodeByte()) :::
      stringMap.keys.toList.flatMap(s => s.toCharArray.map(_.toByte).toList ::: 0.toByte :: Nil)

}

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
}

class Executable[S <: Section:HasName] private(architecture: Architecture, sections: List[S], entryLabel: Label) extends Elf(architecture, sections, entryLabel) {
  override def elfType: ElfType = ElfType.Executable
}

object Executable {
  def apply[S <: Section:HasName](architecture: Architecture, sections: List[S], entryLabel: Label) = new Executable[S](architecture, sections, entryLabel)
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
      endianness.encode(headerSize + headerCount * programHeaderSize)
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

case class ElfVersion private(id: Byte, extended: Int)

case object ElfVersion {
  object Original extends ElfVersion(0x01.toByte, 0x01)
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

case class ElfType private(id: Short)

case object ElfType {
  object Relocatable extends ElfType(0x01.toShort)
  object Executable extends ElfType(0x02.toShort)
  object Shared extends ElfType(0x03.toShort)
  object Core extends ElfType(0x04.toShort)
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
