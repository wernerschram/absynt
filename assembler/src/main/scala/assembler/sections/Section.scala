package assembler.sections

import assembler._
import assembler.reference.RelativeReference

import scala.language.implicitConversions

abstract class Section {
  def content: List[Resource]

  def name: String

  def sectionType: SectionType

  val alignment: Int

  type EncodableCondition = (Resource)=>Boolean

  def contains(label: Label): Boolean = contains((current: Resource) => current.label == label)
  def contains(encodable: Resource): Boolean = contains((current: Resource) => current == encodable)
  def contains(condition: EncodableCondition): Boolean = content.exists(condition)

  def precedingResources(target: Label): List[Resource] =
    content.takeWhile(x => !x.label.matches(target))

  /** returns all resources between a reference and it's target. If it is a back reference, it will include the target
    *
    * @param from
    * @return
    */
  def intermediateEncodables(from: Reference): List[Resource] = {
    val trimLeft = content
      .dropWhile(x => !(x == from || x.label.matches(from.target)))

    if (trimLeft.head == from && trimLeft.head.label.matches(from.target))  // reference to self
      return Nil

    val trimRight = trimLeft.tail
      .takeWhile(x => !(x == from || x.label.matches(from.target)))

    if (trimLeft.head == from)
      trimRight
    else
      trimLeft.head :: trimRight
  }

  def offsetDirection(from: RelativeReference): OffsetDirection = {
    val firstInstruction = content.find(x => x == from || x.label.matches(from.target)).get
    if (firstInstruction.label.matches(from.target))
      if (firstInstruction==from)
        OffsetDirection.Self
      else
        OffsetDirection.Backward
    else
      OffsetDirection.Forward
  }
}

trait LastIteration {
  iteration: Section =>

  def finalContent: List[Resource with Encodable]

  def offset(label: Label): Int =
    finalContent.takeWhile(current => current.label != label)
      .map(_.size).sum

  lazy val encodeByte: List[Byte] = finalContent.flatMap { x => x.encodeByte }

  lazy val size: Int = encodeByte.length
}

case class AlignmentFiller(section: Section) extends DependentResource {
  override def encodeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Encodable =
    EncodedByteList(Seq.fill(sizeForDependencySize(dependencySize, offsetDirection))(0.toByte))(label)

  override def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int = {
    val alignment = dependencySize % section.alignment
    if (alignment != 0)
      section.alignment - alignment
    else 0
  }

  override def possibleSizes: Set[Int] = (0 to section.alignment by 1).toSet

  override def label: Label = Label.noLabel

  override def toString: String = s"filler for ${section.name}"
}

object Section {
  def apply(`type`: SectionType, sectionName: String, resources: List[Resource]): Section =
    new Section {
      val alignment: Int = 16
      override val name: String = sectionName
      override val sectionType: SectionType = `type`
      override val content: List[Resource] =
        AlignmentFiller(this) ::
          resources
    }

  def lastIteration(`type`: SectionType, sectionName: String, encodables: List[Resource with Encodable]):
  Section with LastIteration =
    new Section with LastIteration {
      val alignment: Int = 16
      override val name: String = sectionName
      override val sectionType: SectionType = `type`
      override val finalContent: List[Resource with Encodable] = encodables
      override val content: List[Resource] = finalContent
    }
}

sealed abstract class SectionType private(defaultName: String)

object SectionType {
  object Text extends SectionType(".text")
  object Data extends SectionType(".data")
}

