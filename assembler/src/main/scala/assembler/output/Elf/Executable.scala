package assembler.output.Elf

import assembler._
import assembler.resource._
import assembler.sections.Section

abstract class Elf(
  val architecture: Architecture,
  val applicationSections: List[Section],
  val entryLabel: Label)
  extends Application {

  lazy val sections: List[Section] = applicationSections ::: stringSection :: Nil

  val magic: List[Byte] = 0x7F.toByte :: "ELF".toCharArray.map(_.toByte).toList

  val version: ElfVersion = ElfVersion.Original

  implicit def endianness: Endianness = architecture.endianness

  def elfType: ElfType

  val fileAlignment: Int = 0x1000

  val stringMap: Map[String, Int] =
    stringOffset(("" :: applicationSections.map(s => s.name) ::: StringSectionHeader.name :: Nil).distinct).toMap

  def programHeaders: List[ProgramHeader] =
    sections.init.map(s => ProgramHeader(s, this))

  def sectionHeaders: List[SectionHeader] =
    NullSectionHeader(this) ::
    sections.init.map(s => new SectionSectionHeader(s, this)) :::
    new StringSectionHeader(this) :: Nil

  val stringSectionHeaderIndex: Int = applicationSections.size + 1

  val stringTableSize: Int = stringMap.keys.toList.map(k => k.length + 1).sum // + 1 because they are null terminated

  val programHeaderOffset: Long =
    architecture.processorClass.headerSize

  private val dataOffset: Long =
    programHeaderOffset + applicationSections.size * architecture.processorClass.programHeaderSize

  def sectionFileOffset(section: Section): Long =
    encodableSections.takeWhile(s => s != section).map(_.size).sum + dataOffset

  def stringTableOffset: Long =
    encodableSections.init.map(_.size).sum + dataOffset
//    sectionFileOffset(encodableSections.init.last) + encodableSections.init.last.size

  def sectionHeaderOffset: Long =
    stringTableOffset + stringTableSize

  def stringOffset(strings: List[String]): List[(String, Int)] =
    (strings.head, 0) :: stringOffset(1, strings)

  private def stringOffset(startOffset: Int, strings: List[String]): List[(String, Int)] = strings match {
    case Nil => Nil
    case head :: Nil => (head, startOffset) :: Nil
    case head :: neck :: Nil => (neck, startOffset + head.length) :: Nil
    case head :: neck :: tail => (neck, startOffset + head.length) :: stringOffset(startOffset + head.length + 1, neck :: tail)
  }

  def entryReference: ElfAbsoluteReference = ElfAbsoluteReference(entryLabel, this)

  def resources: Seq[Resource] =
    EncodedByteList(
      magic :::
      architecture.processorClass.id ::
      endianness.id ::
      version.id ::
      architecture.ABI.encodeBytes :::
      endianness.encode(elfType.id) :::
      endianness.encode(architecture.processor.id) :::
      endianness.encode(version.extended)) ::
    entryReference ::
    EncodedByteList(architecture.processorClass.numberBytes(programHeaderOffset)) ::
    ElfSectionHeaderReference(this) ::
    EncodedByteList(
      endianness.encode(architecture.processor.flags) :::
      endianness.encode(architecture.processorClass.headerSize) :::
      endianness.encode(architecture.processorClass.programHeaderSize) :::
      endianness.encode(programHeaders.size.toShort) :::
      endianness.encode(architecture.processorClass.sectionHeaderSize) :::
      endianness.encode(sectionHeaders.size.toShort) :::
      endianness.encode(stringSectionHeaderIndex.toShort))::
    Nil

  override def encodeByte: List[Byte] = {

    val dependentMap: Map[DependentResource, Encodable] =
      encodablesForReferences(
        alignmentFillers.values.toList :::
        resources.collect{case r: DependentResource => r}.toList :::
        programHeaders.flatMap(p => p.resources.collect{case r: DependentResource => r}) :::
        sections.flatMap(s => s.content.collect{case r: DependentResource => r}) :::
        sectionHeaders.flatMap(p => p.resources.collect{case r: DependentResource => r})
      )

    encodableResources(resources, dependentMap).flatMap(_.encodeByte).toList :::
      programHeaders.flatMap(p => encodableResources(p.resources, dependentMap)).flatMap(_.encodeByte) :::
      sections.flatMap(s => encodableSection(s, dependentMap).encodeByte) :::
      sectionHeaders.flatMap(p => encodableResources(p.resources, dependentMap)).flatMap(_.encodeByte)
  }

  override lazy val alignmentFillers: Map[Section, AlignmentFiller] = sections.map(s => s -> ElfAlignmentFiller(s)).toMap

  lazy val stringSection = Section(assembler.sections.SectionType.Data, ".shstrtab",
    EncodedByteList(stringMap.keys.toList.flatMap(s => s.toCharArray.map(_.toByte).toList ::: 0.toByte :: Nil)) :: Nil)
}

case class ElfAlignmentFiller(section: Section) extends AlignmentFiller(Label.noLabel) {
  override def encodableForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Encodable =
    EncodedByteList(Seq.fill(sizeForDependencySize(dependencySize, offsetDirection))(0.toByte))(label)

  override def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int = {
    val alignment = dependencySize % section.alignment
    if (alignment != 0)
      section.alignment - alignment
    else 0
  }

  override def possibleSizes: Set[Int] = (0 to section.alignment by 1).toSet

  override def toString: String = s"filler for ${section.name}"
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

case class ElfAbsoluteReference(override val target: Label, elf: Elf) extends AbsoluteReference(target, Label.noLabel) {
  implicit def endianness: Endianness = elf.endianness

  override def encodableForDistance(distance: Int): Encodable = EncodedByteList(elf.architecture.processorClass.numberBytes(distance))

  override def sizeForDistance(distance: Int): Int = elf.architecture.processorClass.numberSize

  override def possibleSizes: Set[Int] = Set(elf.architecture.processorClass.numberSize)

  override def toString: String = s"Memory address of label: $target"
}

case class ElfSectionFileReference(target: Section, elf: Elf) extends DependentResource(Label.noLabel) {
  implicit def endianness: Endianness = elf.endianness

  override def encodableForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Encodable =
    EncodedByteList(elf.architecture.processorClass.numberBytes(dependencySize))

  override def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int =
    elf.architecture.processorClass.numberSize

  override def possibleSizes: Set[Int] = Set(elf.architecture.processorClass.numberSize)

  override def dependencies(context: Application): (List[Resource], OffsetDirection) =
    (context.sections.takeWhile(s => s != target).flatMap(s => s.content), OffsetDirection.Absolute)

  override def toString: String = s"File address of section: ${target.name}"
}

case class ElfSectionReference(target: Section, elf: Elf) extends DependentResource(Label.noLabel) {
  implicit def endianness: Endianness = elf.endianness

  override def encodableForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Encodable =
    EncodedByteList(elf.architecture.processorClass.numberBytes(dependencySize + elf.startOffset))

  override def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int =
    elf.architecture.processorClass.numberSize

  override def possibleSizes: Set[Int] = Set(elf.architecture.processorClass.numberSize)

  override def dependencies(context: Application): (List[Resource], OffsetDirection) =
    (context.sectionDependencies(target), OffsetDirection.Absolute)

  override def toString: String = s"Memory address of section: ${target.name}"
}

case class ElfSectionSize(target: Section, elf: Elf) extends DependentResource(Label.noLabel) {
  implicit def endianness: Endianness = elf.endianness

  override def encodableForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Encodable =
    EncodedByteList(elf.architecture.processorClass.numberBytes(dependencySize))

  override def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int =
    elf.architecture.processorClass.numberSize

  override def possibleSizes: Set[Int] = Set(elf.architecture.processorClass.numberSize)

  override def dependencies(context: Application): (List[Resource], OffsetDirection) = {
    if (elf.sections.head == target)
      (elf.resources.toList ::: elf.programHeaders.flatMap(_.resources) ::: elf.alignmentFillers(target) :: target.content, OffsetDirection.Absolute)
    else
      (elf.alignmentFillers(target) :: target.content, OffsetDirection.Absolute)
  }

  override def toString: String = s"Size of section: ${target.name}"
}

case class ElfSectionHeaderReference(elf: Elf) extends DependentResource(Label.noLabel) {
  implicit def endianness: Endianness = elf.endianness

  override def encodableForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Encodable =
    EncodedByteList(elf.architecture.processorClass.numberBytes(dependencySize))

  override def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int =
    elf.architecture.processorClass.numberSize

  override def possibleSizes: Set[Int] = Set(elf.architecture.processorClass.numberSize)

  override def dependencies(context: Application): (List[Resource], OffsetDirection) =
    (elf.resources.toList ::: elf.programHeaders.flatMap(_.resources) ::: context.sections.flatMap(s => elf.alignmentFillers(s) :: s.content), OffsetDirection.Absolute)

  override def toString: String = s"Section header reference"
}
