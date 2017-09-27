package assembler.sections

import assembler._
import assembler.reference.{AbsoluteReference, CurrentSection, OtherSection, RelativeReference}

import scala.annotation.tailrec
import scala.language.implicitConversions

trait Section {
  val content: List[Resource]

  def name: String

  def sectionType: SectionType

  val baseAddress: Int

  val alignment: Int

  type EncodableCondition = (Resource)=>Boolean

  def contains(label: Label): Boolean = contains((current: Resource) => current.label == label)
  def contains(encodable: Resource): Boolean = contains((current: Resource) => current == encodable)
  def contains(condition: EncodableCondition): Boolean = content.exists(condition)

  def minimumRelativeAddress(label: Label): Int = minimumRelativeAddress((current: Resource) => current.label == label)
  def minimumRelativeAddress(encodable: Resource): Int = minimumRelativeAddress((current: Resource) => current == encodable)
  def minimumRelativeAddress(condition: EncodableCondition): Int =
    content.takeWhile(current => !condition(current)).map(current => current.minimumSize).sum

  def maximumRelativeAddress(label: Label): Int = maximumRelativeAddress((current: Resource) => current.label == label)
  def maximumRelativeAddress(encodable: Resource): Int = maximumRelativeAddress((current: Resource) => current == encodable)
  def maximumRelativeAddress(condition: EncodableCondition): Int =
    content.takeWhile(current => !condition(current)).map(current => current.maximumSize).sum

  def precedingResources(target: Label): List[Resource] =
    content.takeWhile(x => !x.label.matches(target))

  def intermediateEncodables(from: RelativeReference): List[Resource] = {
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

  def isForwardReference(from: RelativeReference): Boolean = {
    val firstInstruction = content.find(x => x == from || x.label.matches(from.target)).get
    !firstInstruction.label.matches(from.target)
  }

  private def nextContent(currentApplication: Application): List[Resource] = {
    val newContent: List[Resource] = content.map {
      case referencing: RelativeReference =>
        referencing.toInSectionState(this)

      case absolute: AbsoluteReference with OtherSection =>
        val targetSection = currentApplication.sections.find(_.name == absolute.sectionName)
        assume(targetSection.isDefined)
        absolute.toInSectionState(currentApplication)

      case absolute: AbsoluteReference with CurrentSection =>
        absolute.toInSectionState(currentApplication)

      case resource =>
        resource
    }
    newContent
  }

  @tailrec
  final def encodable(currentApplication: Application): Section with LastIteration = {
    val newContent = nextContent(currentApplication)
    if (newContent.forall { case _: Encodable => true; case _ => false }) {
      Section.lastIteration(sectionType, name, newContent.map(r => r.asInstanceOf[Resource with Encodable]), baseAddress)
   } else {
      Section(sectionType, name, newContent, baseAddress).encodable(currentApplication)
    }
  }
}

trait LastIteration {
  iteration: Section =>

  val finalContent: List[Resource with Encodable]

  def relativeAddress(label: Label): Int = relativeAddress((current: Resource) => current.label == label)
  def relativeAddress(encodable: Resource): Int = relativeAddress((current: Resource) => current == encodable)
  def relativeAddress(condition: EncodableCondition): Int =
    finalContent.takeWhile(current => !condition(current)).map(current => current.size).sum

  lazy val encodeByte: List[Byte] = finalContent.flatMap { x => x.encodeByte }

  lazy val size: Int = encodeByte.length
}

object Section {
  def apply(`type`: SectionType, sectionName: String, resources: List[Resource], base: Int): Section =
    new Section {
      val alignment: Int = 16
      override val name: String = sectionName
      override val sectionType: SectionType = `type`
      override val content: List[Resource] = resources
      override val baseAddress: Int = base
    }

  def lastIteration(`type`: SectionType, sectionName: String, encodables: List[Resource with Encodable], base: Int): Section with LastIteration =
    new Section with LastIteration {
      val alignment: Int = 16
      override val name: String = sectionName
      override val sectionType: SectionType = `type`
      override val finalContent: List[Resource with Encodable] = encodables
      override val content: List[Resource] = finalContent
      override val baseAddress: Int = base
    }
}

sealed abstract class SectionType private(defaultName: String)

object SectionType {
  object Text extends SectionType(".text")
  object Data extends SectionType(".data")
}

