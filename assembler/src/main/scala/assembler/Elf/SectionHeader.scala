package assembler.Elf

import assembler.sections.{LastIteration, Section}

abstract class SectionHeader()(implicit elf: Elf) {

  def nameReference: Int
  def `type`: SectionType
  def flags: Flags[SectionFlag]
  def sectionAddress: Long
  def sectionFileOffset: Long
  def segmentFileSize: Long
  def link: Int
  def info: Int

  def alignBytes: Int
  def entrySize: Int

  implicit def endianness: Endianness = elf.endianness

  def header: List[Byte] = {
    elf.endianness.encode(nameReference) :::
    elf.endianness.encode(`type`.id) :::
    elf.architecture.processorClass.flagBytes(flags) :::
    elf.architecture.processorClass.numberBytes(sectionAddress) :::
    elf.architecture.processorClass.numberBytes(sectionFileOffset) :::
    elf.architecture.processorClass.numberBytes(segmentFileSize) :::
    elf.endianness.encode(link) :::
    elf.endianness.encode(info) :::
    elf.architecture.processorClass.numberBytes(alignBytes) :::
    elf.architecture.processorClass.numberBytes(entrySize)
  }
}

class SectionSectionHeader(section: Section with LastIteration)(implicit elf: Elf) extends SectionHeader {

  val nameReference: Int = elf.stringMap(section.name)
  val `type`: SectionType = SectionType.ProgramBits
  val flags: Flags[SectionFlag] =
    section.sectionType match {
      case assembler.sections.SectionType.Text =>
        SectionFlag.Alloc | SectionFlag.ExecutableInstruction
      case assembler.sections.SectionType.Data =>
        SectionFlag.Alloc | SectionFlag.Write
    }
  val sectionAddress: Long = section.baseAddress
  val sectionFileOffset: Long = elf.fileOffset(section)
  val segmentFileSize: Long = section.size
  val link: Int = 0
  val info: Int = 0

  val alignBytes: Int = 0x20

  val entrySize: Int = 0x0
}

class NullSectionHeader()(implicit elf: Elf) extends SectionHeader {
  val nameReference: Int = 0
  val `type`: SectionType = SectionType.Null
  val flags: Flags[SectionFlag] = Flags.None
  val sectionAddress: Long = 0
  val sectionFileOffset: Long = 0
  val segmentFileSize: Long = 0
  val link: Int = 0
  val info: Int = 0

  val alignBytes: Int = 0
  val entrySize: Int = 0
  override def header: List[Byte] = List.fill(elf.architecture.processorClass.sectionHeaderSize)(0.toByte)
}

object NullSectionHeader {
  def apply()(implicit elf: Elf): NullSectionHeader =
    new NullSectionHeader
}

class StringSectionHeader()(implicit elf: Elf) extends SectionHeader {

  val nameReference: Int = elf.stringMap(".shstrtab")
  val `type`: SectionType = SectionType.StringTable
  val flags: Flags[SectionFlag] = Flags.None
  val sectionAddress: Long = 0
  val sectionFileOffset: Long = elf.stringTableOffset
  val segmentFileSize: Long = elf.stringMap.keys.toList.map(k => k.length + 1).sum // + 1 because they are null terminated
  val link: Int = 0
  val info: Int = 0

  val alignBytes: Int = 0x01

  val entrySize: Int = 0x01
}

abstract case class SectionType private(id: Int)

object SectionType {
  object Null extends SectionType(0)
  object ProgramBits extends SectionType(1)
  object SymbolTable extends SectionType(2)
  object StringTable extends SectionType(3)
  //...
}


case class SectionFlag private(flag: Int) extends Flags[SectionFlag] {
  override val encode: Long = flag
}

object SectionFlag {
  object Write extends SectionFlag(0x01.toByte)
  object Alloc extends SectionFlag(0x02.toByte)
  object ExecutableInstruction extends SectionFlag(0x04.toByte)
  object MaskProcessor extends SectionFlag(0x08.toByte)
  //  ...
}
