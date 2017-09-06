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

class Executable[S <: Section:HasName] private(architecture: Architecture, sections: List[S], entryLabel: Label) extends Elf(architecture, sections, entryLabel) {
  override def elfType: ElfType = ElfType.Executable
}

object Executable {
  def apply[S <: Section:HasName](architecture: Architecture, sections: List[S], entryLabel: Label) = new Executable[S](architecture, sections, entryLabel)
}

case class ElfVersion private(id: Byte, extended: Int)

case object ElfVersion {
  object Original extends ElfVersion(0x01.toByte, 0x01)
}


case class ElfType private(id: Short)

case object ElfType {
  object Relocatable extends ElfType(0x01.toShort)
  object Executable extends ElfType(0x02.toShort)
  object Shared extends ElfType(0x03.toShort)
  object Core extends ElfType(0x04.toShort)
}

