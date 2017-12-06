package assembler.output.Elf

import assembler._
import assembler.reference.{AbsoluteReference, RelativeReference}
import assembler.sections.{AlignmentFiller, LastIteration, Section}

abstract class Elf(
  val architecture: Architecture,
  sections: List[Section],
  val entryLabel: Label)
  extends Application(sections) {

  val magic: List[Byte] = 0x7F.toByte :: Nil ::: "ELF".toCharArray.map(_.toByte).toList

  val version: ElfVersion = ElfVersion.Original

  implicit def endianness: Endianness = architecture.endianness

  def elfType: ElfType

  val fileAlignment: Int = 0x1000

  val stringMap: Map[String, Int] =
    stringOffset(("" :: sections.map(s => s.name) ::: StringSectionHeader.name :: Nil).distinct).toMap

  val programHeaders: List[ProgramHeader] =
    encodableSections.map(s => ProgramHeader(s, this))

  def sectionHeaders: List[SectionHeader] =
    NullSectionHeader(this) ::
    encodableSections.map(s => new SectionSectionHeader(s, this)) :::
    new StringSectionHeader(this) :: Nil

  val stringSectionHeaderIndex: Int = sectionHeaders.size - 1 // last section

  val stringTableSize: Int = stringMap.keys.toList.map(k => k.length + 1).sum // + 1 because they are null terminated

  val programHeaderOffset: Long =
    architecture.processorClass.headerSize

  private val dataOffset: Long =
    programHeaderOffset + programHeaders.size * architecture.processorClass.programHeaderSize

  def sectionFileOffset(section: Section): Long = encodableSections.takeWhile(s => s!=section).foldLeft(dataOffset) {
      (dataOffset, nextSection) => align(dataOffset, nextSection.alignment) + nextSection.size
    }

  def alignedSectionOffset(section: Section): Long = align(sectionFileOffset(section), section.alignment)

  private def align(value: Long, alignment: Int): Long = if (value % alignment == 0)
    value
  else
    value + alignment - value % alignment

  val stringTableOffset: Long =
    alignedSectionOffset(encodableSections.last) + encodableSections.last.size

  val sectionHeaderOffset: Long =
    stringTableOffset + stringTableSize

  //TODO review all alignment fixes. The AlignmentFiller should make all alignment implicit

  def stringOffset(strings: List[String]): List[(String, Int)] =
    (strings.head, 0) :: stringOffset(1, strings)

  private def stringOffset(startOffset: Int, strings: List[String]): List[(String, Int)] = strings match {
    case Nil => Nil
    case head :: Nil => (head, startOffset) :: Nil
    case head :: neck :: Nil => (neck, startOffset + head.length) :: Nil
    case head :: neck :: tail => (neck, startOffset + head.length) :: stringOffset(startOffset + head.length + 1, neck :: tail)
  }

  private def alignSectionData(offset: Long, section: Section with LastIteration): List[Byte] = {
    val prefix: List[Byte] = if (offset % section.alignment != 0)
      List.fill(section.alignment - offset.toInt % section.alignment)(0)
    else
      Nil
    prefix ::: section.encodeByte
  }

  val alignedSectionData: List[List[Byte]] = encodableSections.map(s => alignSectionData(sectionFileOffset(s), s))

  override def encodeByte: List[Byte] =
    magic :::
    architecture.processorClass.id ::
    endianness.id ::
    version.id ::
    architecture.ABI.encodeBytes :::
    endianness.encode(elfType.id) :::
    endianness.encode(architecture.processor.id) :::
    endianness.encode(version.extended) :::
    architecture.processorClass.numberBytes(getAbsoluteOffset(entryLabel)) :::
    architecture.processorClass.numberBytes(programHeaderOffset) :::
    architecture.processorClass.numberBytes(sectionHeaderOffset) :::
    endianness.encode(architecture.processor.flags) :::
    endianness.encode(architecture.processorClass.headerSize) :::
    endianness.encode(architecture.processorClass.programHeaderSize) :::
    endianness.encode(programHeaders.size.toShort) :::
    endianness.encode(architecture.processorClass.sectionHeaderSize) :::
    endianness.encode(sectionHeaders.size.toShort) :::
    endianness.encode(stringSectionHeaderIndex.toShort) :::
    programHeaders.flatMap(p => p.encodeByte) :::
    alignedSectionData.flatten :::
    stringMap.keys.toList.flatMap(s => s.toCharArray.map(_.toByte).toList ::: 0.toByte :: Nil) :::
    sectionHeaders.flatMap(s => s.encodeByte)
}

class Executable private(
  architecture: Architecture,
  sections: List[Section],
  entryLabel: Label,
  override val startOffset: Int)
  extends Elf(architecture, sections, entryLabel) {

  override def elfType: ElfType = ElfType.Executable
}

object Executable {
  def apply(architecture: Architecture, sections: List[Section], entryLabel: Label, startOffset: Int) =
    new Executable(architecture, sections, entryLabel, startOffset)
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

