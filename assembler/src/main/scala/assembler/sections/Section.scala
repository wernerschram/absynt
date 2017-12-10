package assembler.sections

import assembler._
import assembler.resource.{Encodable, RelativeReference, Resource}

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
    content.takeWhile(_.label != target)

  /** returns all resources between a reference and it's target. If it is a back reference, it will include the target
    *
    * @param from
    * @return
    */
  def intermediateResources(from: RelativeReference): List[Resource] = {
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


object Section {
  def apply(`type`: SectionType, sectionName: String, resources: List[Resource]): Section =
    new Section {
      val alignment: Int = 16
      override val name: String = sectionName
      override val sectionType: SectionType = `type`
      override val content: List[Resource] =
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

