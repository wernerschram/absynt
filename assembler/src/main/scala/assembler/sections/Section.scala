package assembler.sections

import assembler._
import assembler.resource.{Labeled, RelativeReference, Resource}

import scala.language.implicitConversions

abstract class Section(val alignment: Int) {
  def content: Seq[Resource]

  def name: String

  def sectionType: SectionType

  def precedingResources(target: Label): Seq[Resource] = content.takeWhile(!matchLabel(_, target))

  private def matchResourceOrLabel(resource: Resource, target: Resource, label: Label): Boolean =
    (resource == target) || (resource match {
      case l: Labeled => l.label.matches(label) || l.resource == target
      case _ => false
    })

  private def matchResourceAndLabel(resource: Resource, target: Resource, label: Label): Boolean =
    resource match {
      case l: Labeled => l.label.matches(label) && l.resource == target
      case _ => false
    }

  private def matchLabel(resource: Resource, label: Label): Boolean =
    resource match {
      case l: Labeled => l.label.matches(label)
      case _ => false
    }

  private def matchResource(resource: Resource, target: Resource): Boolean =
    (resource == target) || (resource match {
      case l: Labeled => l.resource == target
      case _ => false
    })

  /** returns all resources between a relative reference and it's target. If it is a back reference, it will include the target
    *
    * @param from the source relative reference
    * @return the intermediate resources
    */
  def intermediateResources(from: RelativeReference): Seq[Resource] = {

    val trimLeft = content
      .dropWhile(x => !matchResourceOrLabel(x, from, from.target))

    if (matchResourceAndLabel(trimLeft.head, from, from.target))  // reference to self
      return Nil

    val trimRight = trimLeft.tail
      .takeWhile(x => !matchResourceOrLabel(x, from, from.target))

    if (matchResource(trimLeft.head, from))
      trimRight
    else
      trimLeft.head +: trimRight
  }

  def offsetDirection(from: RelativeReference): OffsetDirection = {
    val firstInstruction = content.find(x => matchResourceOrLabel(x, from, from.target)).get
    if (matchLabel(firstInstruction, from.target))
      if (matchResource(firstInstruction, from))
        OffsetDirection.Self
      else
        OffsetDirection.Backward
    else
      OffsetDirection.Forward
  }
}

object Section {
  def apply(`type`: SectionType, sectionName: String, resources: Seq[Resource], alignment: Int = 16): Section =
    new Section(alignment) {
      override val name: String = sectionName
      override val sectionType: SectionType = `type`
      override val content: Seq[Resource] =
          resources
    }

  def text(resources: Seq[Resource], sectionName: String = ".text", alignment: Int = 16): Section =
    apply(SectionType.Text, sectionName, resources, alignment)

  def data(resources: Seq[Resource], sectionName: String = ".data", alignment: Int = 16): Section =
    apply(SectionType.Data, sectionName, resources, alignment)
}

sealed abstract class SectionType private(defaultName: String)

object SectionType {
  object Text extends SectionType(".text")
  object Data extends SectionType(".data")
}

