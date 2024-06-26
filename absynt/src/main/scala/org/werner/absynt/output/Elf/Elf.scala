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

import org.werner.absynt._
import org.werner.absynt.resource._
import org.werner.absynt.sections.Section
import org.werner.absynt.resource.EncodableConversion._
import org.werner.absynt.resource.{Resource, UnlabeledEncodable}

abstract class Elf(
  val architecture: Architecture,
  val applicationSections: Seq[Section],
  val entryLabel: Label)
  extends Application {

  lazy val sections: Seq[Section] = applicationSections :+ stringSection

  val magic: Seq[Byte] = 0x7F.toByte +: "ELF".toIndexedSeq.map(_.toByte)

  val version: ElfVersion = ElfVersion.Original

  given endianness: Endianness = architecture.endianness

  def elfType: ElfType

  val fileAlignment: Int = 0x1000

  val stringMap: Map[String, Int] = (applicationSections.map(s => s.name) :+ StringSectionHeader.name)
    .distinct.foldLeft(Seq(("", 0)))((accumulated, current) =>
      (current, accumulated.head._2 + accumulated.head._1.length + 1) +: accumulated).toMap

  val programHeaders: Seq[ProgramHeader] =
    sections.init.map(s => ProgramHeader(s, this))

  val sectionHeaders: Seq[SectionHeader] =
    NullSectionHeader(this) +:
    sections.init.map(s => new SectionSectionHeader(s, this)) :+
    new StringSectionHeader(this)

  val stringSectionHeaderIndex: Int = applicationSections.size + 1

  val stringTableSize: Int = stringMap.keys.map(k => k.length + 1).sum // + 1 because they are null terminated

  val programHeaderOffset: Long =
    architecture.processorClass.headerSize

  override def sectionDependencies(section: Section): Seq[Resource] =
    applicationHeader ++ programHeaders.flatMap(_.resources) ++
      sections.takeWhile(_ != section).flatMap(s => alignmentFillers(s) +: s.content :+ EncodedBytes(Seq.fill(0x1000)(0.toByte)))

  override def alignedSectionDependencies(section: Section): Seq[Resource] =
    sectionDependencies(section) :+ alignmentFillers(section)

  val applicationHeader: Seq[Resource] =
    Seq(
      EncodedBytes(magic),
      EncodedBytes(architecture.processorClass.id),
      EncodedBytes(endianness.id),
      EncodedBytes(version.id),
      EncodedBytes(architecture.ABI.encodeBytes),
      EncodedBytes(endianness.encode(elfType.id)),
      EncodedBytes(endianness.encode(architecture.processor.id)),
      EncodedBytes(endianness.encode(version.extended)),
      ElfAbsoluteReference(entryLabel, this),
      EncodedBytes(architecture.processorClass.numberBytes(programHeaderOffset)),
      ElfSectionHeaderReference(this),
      EncodedBytes(endianness.encode(architecture.processor.flags)),
      EncodedBytes(endianness.encode(architecture.processorClass.headerSize)),
      EncodedBytes(endianness.encode(architecture.processorClass.programHeaderSize)),
      EncodedBytes(endianness.encode(programHeaders.size.toShort)),
      EncodedBytes(endianness.encode(architecture.processorClass.sectionHeaderSize)),
      EncodedBytes(endianness.encode(sectionHeaders.size.toShort)),
      EncodedBytes(endianness.encode(stringSectionHeaderIndex.toShort))
    )

  val resources: Seq[Resource] =
    applicationHeader ++ programHeaders.flatMap(_.resources) ++
      sections.flatMap(s => alignmentFillers(s) +: s.content) ++
      sectionHeaders.flatMap(p => p.resources)

  lazy val encodeByte: Seq[Byte] =
    resources.encodables(encodablesForDependencies(resources.dependentResources)).encodeByte

  override lazy val alignmentFillers: Map[Section, AlignmentFiller] = sections.map(s => s -> AlignmentFiller(s)).toMap

  lazy val stringSection: Section = Section.data(
    EncodedBytes(stringMap.toSeq.sortBy(_._2).map(_._1).flatMap(s => s.toCharArray.map(_.toByte).toList ::: 0.toByte :: Nil)) :: Nil,
    ".shstrtab", 1)
}

class Executable private(
  architecture: Architecture,
  sections: Seq[Section],
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

case class ElfType private(id: Short, name: String) {
  override def toString: String = name
}

case object ElfType {
  object Relocatable extends ElfType(0x01.toShort, "Relocatable")
  object Executable extends ElfType(0x02.toShort, "Executable")
  object Shared extends ElfType(0x03.toShort, "Shared")
  object Core extends ElfType(0x04.toShort, "Core")
}

case class ElfAbsoluteReference(override val target: Label, elf: Elf) extends AbsoluteReference(target) {
  given endianness: Endianness = elf.endianness

  override def encodableForDistance(distance: Int): UnlabeledEncodable =
    EncodedBytes(elf.architecture.processorClass.numberBytes(distance))

  override def sizeForDistance(distance: Int): Int = elf.architecture.processorClass.numberSize

  override def possibleSizes: Set[Int] = Set(elf.architecture.processorClass.numberSize)

  override def toString: String = s"Memory address of label: $target"
}

case class ElfSectionFileReference(target: Section, elf: Elf) extends UnlabeledDependentResource {
  given endianness: Endianness = elf.endianness

  override def unlabeledForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): UnlabeledEncodable =
    EncodedBytes(elf.architecture.processorClass.numberBytes(dependencySize))

  override def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int =
    elf.architecture.processorClass.numberSize

  override def possibleSizes: Set[Int] = Set(elf.architecture.processorClass.numberSize)

  override def dependencies(context: Application): (Seq[Resource], OffsetDirection) =
    if elf.sections.head != target then
      (((elf.applicationHeader ++ elf.programHeaders.flatMap(_.resources)) :+ context.alignmentFillers(target)) ++
        context.sections.takeWhile(_ != target).flatMap(s => context.alignmentFillers(s) +: s.content)
      , OffsetDirection.Absolute)
    else
     (Nil, OffsetDirection.Absolute)

  override def toString: String = s"File address of section: ${target.name}"
}

case class ElfSectionReference(target: Section, elf: Elf) extends UnlabeledDependentResource {
  given endianness: Endianness = elf.endianness

  override def unlabeledForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): UnlabeledEncodable =
    EncodedBytes(elf.architecture.processorClass.numberBytes(dependencySize))

  override def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int =
    elf.architecture.processorClass.numberSize

  override def possibleSizes: Set[Int] = Set(elf.architecture.processorClass.numberSize)

  override def dependencies(context: Application): (Seq[Resource], OffsetDirection) =
    if elf.sections.head == target then // First section includes the application header and the program headers
      (Seq(context.startFiller), OffsetDirection.Absolute)
    else
      (context.startFiller +: context.alignedSectionDependencies(target), OffsetDirection.Absolute)

  override def toString: String = s"Memory address of section: ${target.name}"
}

case class ElfSectionSize(target: Section, elf: Elf) extends UnlabeledDependentResource {
  given endianness: Endianness = elf.endianness

  override def unlabeledForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): UnlabeledEncodable =
    EncodedBytes(elf.architecture.processorClass.numberBytes(dependencySize))

  override def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int =
    elf.architecture.processorClass.numberSize

  override def possibleSizes: Set[Int] = Set(elf.architecture.processorClass.numberSize)

  override def dependencies(context: Application): (Seq[Resource], OffsetDirection) = {
    if elf.sections.head == target then // First section includes the application header and the program headers
      (elf.applicationHeader ++ elf.programHeaders.flatMap(_.resources) ++ target.content :+ elf.alignmentFillers(target), OffsetDirection.Absolute)
    else
      (target.content, OffsetDirection.Absolute)
  }

  override def toString: String = s"Size of section: ${target.name}"
}

case class ElfSectionHeaderReference(elf: Elf) extends UnlabeledDependentResource {
  given endianness: Endianness = elf.endianness

  override def unlabeledForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): UnlabeledEncodable =
    EncodedBytes(elf.architecture.processorClass.numberBytes(dependencySize))

  override def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int =
    elf.architecture.processorClass.numberSize

  override def possibleSizes: Set[Int] = Set(elf.architecture.processorClass.numberSize)

  override def dependencies(context: Application): (Seq[Resource], OffsetDirection) =
    (elf.applicationHeader ++ (elf.programHeaders.flatMap(_.resources) ++
      elf.sections.flatMap(s => elf.alignmentFillers(s) +: s.content)), OffsetDirection.Absolute)

  override def toString: String = s"Section header reference"
}
