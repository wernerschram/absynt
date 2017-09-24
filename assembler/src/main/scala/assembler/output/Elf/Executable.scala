package assembler.output.Elf

import assembler._
import assembler.sections.Section

abstract class Elf(val architecture: Architecture, sections: List[Section], val entryLabel: Label) extends Application(sections) {
  val magic: List[Byte] = 0x7F.toByte :: Nil ::: "ELF".toCharArray.map(_.toByte).toList
  val version: ElfVersion = ElfVersion.Original

  implicit def endianness: Endianness = architecture.endianness

  def elfType: ElfType

  val stringMap: Map[String, Int] =
    stringOffset(("" :: sections.map(s => s.name) ::: ".shstrtab" :: Nil )
       .distinct).toMap

  val programHeaders: List[ProgramHeader] =
    encodableSections.map(s => ProgramHeader(s, this))

  val sectionHeaders: List[SectionHeader] =
    NullSectionHeader(this) ::
    encodableSections.map(s => new SectionSectionHeader(s, this)) :::
    new StringSectionHeader(this) :: Nil

  val sectionNamesSectionHeaderIndex: Int = sectionHeaders.size - 1 // last section

  val programHeadersSize: Long = programHeaders.size * architecture.processorClass.programHeaderSize
  val sectionsSize: Long = encodableSections.map(s => s.size).sum
  val stringTableSize: Int = stringMap.keys.toList.map(k => k.length + 1).sum // + 1 because they are null terminated

  val programHeaderOffset: Long =
    architecture.processorClass.headerSize

  val dataOffset: Long =
    programHeaderOffset + programHeaders.size * architecture.processorClass.programHeaderSize

  def fileOffset(section: Section): Long =
    dataOffset + encodableSections.takeWhile(s => s != section).map(s => s.size).sum

  val stringTableOffset: Long =
    dataOffset + sectionsSize

  val sectionHeaderOffset: Long =
    stringTableOffset + stringTableSize

  def memoryAddress(section: Section): Long = section.baseAddress

  def memoryOffset(section: Section): Long = 0

  override def getAbsoluteAddress(encodable: Resource): Long = {
    encodableSections.filter(s=> s.contains(encodable))
      .map(s => memoryAddress(s) + s.relativeAddress(encodable)).head
  }

  override def getAbsoluteAddress(label: Label): Long =
    encodableSections.filter(s => s.contains(label))
      .map(s => memoryAddress(s) + s.relativeAddress(label)).head

  def stringOffset(strings: List[String]): List[(String, Int)] =
    (strings.head, 0) :: stringOffset(1, strings)

  private def stringOffset(startOffset: Int, strings: List[String]): List[(String, Int)] = strings match {
    case Nil => Nil
    case head :: Nil => (head, startOffset) :: Nil
    case head :: neck :: Nil => (neck, startOffset + head.length) :: Nil
    case head :: neck :: tail => (neck, startOffset + head.length) :: stringOffset(startOffset + head.length + 1, neck :: tail)
  }

  override def encodeByte: List[Byte] =
      magic :::
      architecture.processorClass.id ::
      endianness.id ::
      version.id ::
      architecture.ABI.encodeBytes :::
      endianness.encode(elfType.id) :::
      endianness.encode(architecture.processor.id) :::
      endianness.encode(version.extended) :::
      architecture.processorClass.numberBytes(getAbsoluteAddress(entryLabel)) :::
      architecture.processorClass.numberBytes(programHeaderOffset) :::
      architecture.processorClass.numberBytes(sectionHeaderOffset) :::
      endianness.encode(architecture.processor.flags) :::
      endianness.encode(architecture.processorClass.headerSize) :::
      endianness.encode(architecture.processorClass.programHeaderSize) :::
      endianness.encode(programHeaders.size.toShort) :::
      endianness.encode(architecture.processorClass.sectionHeaderSize) :::
      endianness.encode(sectionHeaders.size.toShort) :::
      endianness.encode(sectionNamesSectionHeaderIndex.toShort) :::
      programHeaders.flatMap(p => p.encodeByte) :::
      encodableSections.flatMap(s => s.encodeByte) :::
      stringMap.keys.toList.flatMap(s => s.toCharArray.map(_.toByte).toList ::: 0.toByte :: Nil) :::
      sectionHeaders.flatMap(s => s.encodeByte)
}

class Executable private(architecture: Architecture, sections: List[Section], entryLabel: Label)
  extends Elf(architecture, sections, entryLabel) {
  override def elfType: ElfType = ElfType.Executable

}

object Executable {
  def apply(architecture: Architecture, sections: List[Section], entryLabel: Label) =
    new Executable(architecture, sections, entryLabel)
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

