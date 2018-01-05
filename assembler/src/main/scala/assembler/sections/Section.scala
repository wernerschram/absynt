package assembler.sections

import assembler._
import assembler.resource.{RelativeReference, Resource}

import scala.language.implicitConversions

abstract class Section(val alignment: Int) {
  def content: List[Resource]

  def name: String

  def sectionType: SectionType

  def contains(label: Label): Boolean = content.exists(_.label == label)
  def contains(encodable: Resource): Boolean = content.contains(encodable)

  def precedingResources(target: Label): List[Resource] = content.takeWhile(_.label != target)

  /** returns all resources between a relative reference and it's target. If it is a back reference, it will include the target
    *
    * @param from the source relative reference
    * @return the intermediate resources
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

object Section {
  def apply(`type`: SectionType, sectionName: String, resources: List[Resource], alignment: Int = 16): Section =
    new Section(alignment) {
      override val name: String = sectionName
      override val sectionType: SectionType = `type`
      override val content: List[Resource] =
          resources
    }
}

sealed abstract class SectionType private(defaultName: String)

object SectionType {
  object Text extends SectionType(".text")
  object Data extends SectionType(".data")
}

