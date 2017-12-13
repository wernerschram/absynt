package assembler.output.Elf

import assembler.EncodedByteList
import assembler.resource.Resource
import assembler.sections.Section

abstract class SectionHeader(elf: Elf) {

  def nameReference: Int
  def `type`: SectionType
  def flags: Flags[SectionFlag]
  def sectionReference: Resource
  def sectionFileReference: Resource
  def sectionFileSize: Resource
  def link: Int
  def info: Int

  def alignBytes: Int
  def entrySize: Int

  implicit def endianness: Endianness = elf.endianness

  def resources: Seq[Resource] =
    EncodedByteList(
      elf.endianness.encode(nameReference) :::
      elf.endianness.encode(`type`.id) :::
      elf.architecture.processorClass.flagBytes(flags)) ::
    sectionReference ::
    sectionFileReference ::
    sectionFileSize ::
    EncodedByteList(
      elf.endianness.encode(link) :::
      elf.endianness.encode(info) :::
      elf.architecture.processorClass.numberBytes(alignBytes) :::
      elf.architecture.processorClass.numberBytes(entrySize)) ::
    Nil

  def encodeByte: List[Byte] = {
    elf.endianness.encode(nameReference) :::
    elf.endianness.encode(`type`.id) :::
    elf.architecture.processorClass.flagBytes(flags) :::
    elf.endianness.encode(link) :::
    elf.endianness.encode(info) :::
    elf.architecture.processorClass.numberBytes(alignBytes) :::
    elf.architecture.processorClass.numberBytes(entrySize)
  }
}

class SectionSectionHeader(section: Section, elf: Elf) extends SectionHeader(elf) {

  val nameReference: Int = elf.stringMap(section.name)
  val `type`: SectionType = SectionType.ProgramBits
  val flags: Flags[SectionFlag] =
    section.sectionType match {
      case assembler.sections.SectionType.Text =>
        SectionFlag.Alloc | SectionFlag.ExecutableInstruction
      case assembler.sections.SectionType.Data =>
        SectionFlag.Alloc | SectionFlag.Write
    }

  override def sectionFileSize: Resource =  ElfSectionSize(section, elf)
  val link: Int = 0
  val info: Int = 0

  val alignBytes: Int = section.alignment

  val entrySize: Int = 0x0

  override def sectionReference: Resource = ElfSectionReference(section, elf)
  override def sectionFileReference: Resource = ElfSectionFileReference(section, elf)
}

class NullSectionHeader(elf: Elf)
  extends SectionHeader(elf) {
  val nameReference: Int = 0
  val `type`: SectionType = SectionType.Null
  val flags: Flags[SectionFlag] = Flags.None
  val sectionAddress: Option[Long] = None
  val link: Int = 0
  val info: Int = 0

  val alignBytes: Int = 0
  val entrySize: Int = 0
  override def encodeByte: List[Byte] = List.fill(elf.architecture.processorClass.sectionHeaderSize)(0.toByte)

  override def sectionFileSize: Resource = EncodedByteList(elf.architecture.processorClass.numberBytes(0))
  override def sectionFileReference: Resource = EncodedByteList(elf.architecture.processorClass.numberBytes(0))
  override def sectionReference: Resource = EncodedByteList(elf.architecture.processorClass.numberBytes(0))
}

object NullSectionHeader {
  def apply(elf: Elf):
  NullSectionHeader = new NullSectionHeader(elf)
}

class StringSectionHeader(elf: Elf)
  extends SectionHeader(elf) {

  val nameReference: Int = elf.stringMap(StringSectionHeader.name)
  val `type`: SectionType = SectionType.StringTable
  val flags: Flags[SectionFlag] = Flags.None
  val sectionAddress: Option[Long] = None
  val sectionFileOffset: Long = elf.stringTableOffset
  val link: Int = 0
  val info: Int = 0

  val alignBytes: Int = 0x01

  val entrySize: Int = 0x01

  override def sectionFileSize: Resource =  ElfSectionSize(elf.stringSection, elf)
  override def sectionFileReference: Resource = ElfSectionFileReference(elf.stringSection, elf)
  override def sectionReference: Resource = ElfSectionReference(elf.stringSection, elf)
}

object StringSectionHeader {
  val name = ".shstrtab"
  def apply(elf: Elf):
  StringSectionHeader = new StringSectionHeader(elf)
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
