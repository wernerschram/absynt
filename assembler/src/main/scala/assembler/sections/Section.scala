package assembler.sections

import assembler._
import assembler.reference.{AbsoluteReference, RelativeReference}

import scala.annotation.tailrec
import scala.language.implicitConversions

abstract class Section[OffsetType<:Offset:OffsetFactory] {
  val content: List[Resource]

  def name: String

  def sectionType: SectionType

  val alignment: Int

  protected def offset(value: Long): OffsetType = implicitly[OffsetFactory[OffsetType]].offset(value)

  type EncodableCondition = (Resource)=>Boolean

  def contains(label: Label): Boolean = contains((current: Resource) => current.label == label)
  def contains(encodable: Resource): Boolean = contains((current: Resource) => current == encodable)
  def contains(condition: EncodableCondition): Boolean = content.exists(condition)

  def minimumOffset(label: Label): OffsetType = minimumOffset((current: Resource) => current.label == label)
  def minimumOffset(encodable: Resource): OffsetType = minimumOffset((current: Resource) => current == encodable)
  def minimumOffset(condition: EncodableCondition): OffsetType =
    offset(content.takeWhile(current => !condition(current)).map(current => current.minimumSize).sum)

  def maximumOffset(label: Label): OffsetType = maximumOffset((current: Resource) => current.label == label)
  def maximumOffset(encodable: Resource): OffsetType = maximumOffset((current: Resource) => current == encodable)
  def maximumOffset(condition: EncodableCondition): OffsetType =
    offset(content.takeWhile(current => !condition(current)).map(current => current.maximumSize).sum)

  def precedingResources(target: Label): List[Resource] =
    content.takeWhile(x => !x.label.matches(target))


  /** returns all resources between a reference and it's target. If it is a back reference, it will include the target
    *
    * @param from
    * @return
    */
  def intermediateEncodables(from: RelativeReference[OffsetType]): List[Resource] = {
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

  def offsetDirection(from: RelativeReference[OffsetType]): OffsetDirection = {
    val firstInstruction = content.find(x => x == from || x.label.matches(from.target)).get
    if (firstInstruction.label.matches(from.target))
      if (firstInstruction==from)
        OffsetDirection.None
      else
        OffsetDirection.Backward
    else
      OffsetDirection.Forward
  }

  private def nextContent[AddressType<:Address[OffsetType]](currentApplication: Application[OffsetType, AddressType]): List[Resource] = {
    val newContent: List[Resource] = content.map {
      case referencing: RelativeReference[OffsetType] =>
        referencing.toInSectionState(this)

      case absolute: AbsoluteReference[OffsetType, AddressType] =>
        absolute.toInSectionState(currentApplication)

      case resource =>
        resource
    }
    newContent
  }

  @tailrec
  final def encodable[AddressType<:Address[OffsetType]](currentApplication: Application[OffsetType, AddressType]): Section[OffsetType] with LastIteration[OffsetType] = {
    val newContent = nextContent(currentApplication)
    if (newContent.forall { case _: Encodable => true; case _ => false }) {
      Section.lastIteration(sectionType, name, newContent.map(r => r.asInstanceOf[Resource with Encodable]))
   } else {
      Section(sectionType, name, newContent).encodable(currentApplication)
    }
  }
}

trait LastIteration[OffsetType<:Offset] {
  iteration: Section[OffsetType] =>

  val finalContent: List[Resource with Encodable]

  def offset(label: Label): OffsetType = offset((current: Resource) => current.label == label)
  def offset(encodable: Resource): OffsetType = offset((current: Resource) => current == encodable)
  def offset(condition: EncodableCondition): OffsetType =
    iteration.offset(finalContent.takeWhile(current => !condition(current)).map(current => current.size).sum)

  lazy val encodeByte: List[Byte] = finalContent.flatMap { x => x.encodeByte }

  lazy val size: Int = encodeByte.length
}

object Section {
  def apply[OffsetType<:Offset:OffsetFactory](`type`: SectionType, sectionName: String, resources: List[Resource]): Section[OffsetType] =
    new Section[OffsetType] {
      val alignment: Int = 16
      override val name: String = sectionName
      override val sectionType: SectionType = `type`
      override val content: List[Resource] = resources
    }

  def lastIteration[OffsetType<:Offset:OffsetFactory](`type`: SectionType, sectionName: String, encodables: List[Resource with Encodable]):
  Section[OffsetType] with LastIteration[OffsetType] =
    new Section[OffsetType] with LastIteration[OffsetType] {
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

