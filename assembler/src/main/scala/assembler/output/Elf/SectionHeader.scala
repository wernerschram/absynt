package assembler.output.Elf

import assembler.{Address, Offset}
import assembler.sections.{LastIteration, Section}

abstract class SectionHeader[OffsetType<:Offset, AddressType<:Address[OffsetType]](elf: Elf[OffsetType, AddressType]) {

  def nameReference: Int
  def `type`: SectionType
  def flags: Flags[SectionFlag]
  def sectionAddress: Option[Address[OffsetType]]
  private def sectionAddressBytes = sectionAddress match {
    case Some(address) => elf.architecture.processorClass.numberBytes(address.toLong)
    case None => List.fill(8)(0.toByte)
  }
  def sectionFileOffset: Long
  def segmentFileSize: Long
  def link: Int
  def info: Int

  def alignBytes: Int
  def entrySize: Int

  implicit def endianness: Endianness = elf.endianness

  def encodeByte: List[Byte] = {
    elf.endianness.encode(nameReference) :::
    elf.endianness.encode(`type`.id) :::
    elf.architecture.processorClass.flagBytes(flags) :::
    sectionAddressBytes :::
    elf.architecture.processorClass.numberBytes(sectionFileOffset) :::
    elf.architecture.processorClass.numberBytes(segmentFileSize) :::
    elf.endianness.encode(link) :::
    elf.endianness.encode(info) :::
    elf.architecture.processorClass.numberBytes(alignBytes) :::
    elf.architecture.processorClass.numberBytes(entrySize)
  }
}

class SectionSectionHeader[OffsetType<:Offset, AddressType<:Address[OffsetType]](section: Section[OffsetType]
  with LastIteration[OffsetType], elf: Elf[OffsetType, AddressType]) extends SectionHeader[OffsetType, AddressType](elf) {

  val nameReference: Int = elf.stringMap(section.name)
  val `type`: SectionType = SectionType.ProgramBits
  val flags: Flags[SectionFlag] =
    section.sectionType match {
      case assembler.sections.SectionType.Text =>
        SectionFlag.Alloc | SectionFlag.ExecutableInstruction
      case assembler.sections.SectionType.Data =>
        SectionFlag.Alloc | SectionFlag.Write
    }
  val sectionAddress: Option[Address[OffsetType]] = Some(elf.memoryAddress(section))
  val sectionFileOffset: Long = elf.alignedSectionOffset(section)

  val segmentFileSize: Long = section.size
  val link: Int = 0
  val info: Int = 0

  val alignBytes: Int = section.alignment

  val entrySize: Int = 0x0
}

class NullSectionHeader[OffsetType<:Offset, AddressType<:Address[OffsetType]](elf: Elf[OffsetType, AddressType])
  extends SectionHeader[OffsetType, AddressType](elf) {
  val nameReference: Int = 0
  val `type`: SectionType = SectionType.Null
  val flags: Flags[SectionFlag] = Flags.None
  val sectionAddress: Option[Address[OffsetType]] = None
  val sectionFileOffset: Long = 0
  val segmentFileSize: Long = 0
  val link: Int = 0
  val info: Int = 0

  val alignBytes: Int = 0
  val entrySize: Int = 0
  override def encodeByte: List[Byte] = List.fill(elf.architecture.processorClass.sectionHeaderSize)(0.toByte)
}

object NullSectionHeader {
  def apply[OffsetType<:Offset, AddressType<:Address[OffsetType]](elf: Elf[OffsetType, AddressType]):
  NullSectionHeader[OffsetType, AddressType] = new NullSectionHeader[OffsetType, AddressType](elf)
}

class StringSectionHeader[OffsetType<:Offset, AddressType<:Address[OffsetType]](elf: Elf[OffsetType, AddressType])
  extends SectionHeader(elf) {

  val nameReference: Int = elf.stringMap(StringSectionHeader.name)
  val `type`: SectionType = SectionType.StringTable
  val flags: Flags[SectionFlag] = Flags.None
  val sectionAddress: Option[Address[OffsetType]] = None
  val sectionFileOffset: Long = elf.stringTableOffset
  val segmentFileSize: Long = elf.stringTableSize
  val link: Int = 0
  val info: Int = 0

  val alignBytes: Int = 0x01

  val entrySize: Int = 0x01
}

object StringSectionHeader {
  val name = ".shstrtab"
  def apply[OffsetType<:Offset, AddressType<:Address[OffsetType]](elf: Elf[OffsetType, AddressType]):
  StringSectionHeader[OffsetType, AddressType] = new StringSectionHeader(elf)
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
