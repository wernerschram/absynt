package assembler.sections

import assembler._
import assembler.reference.{AbsoluteReference, SinglePassRelativeReference}

import scala.annotation.tailrec
import scala.language.implicitConversions

abstract class Section[OffsetType<:Offset:OffsetFactory] {
  def content: List[Resource]

  def name: String

  def sectionType: SectionType

  val alignment: Int

  protected def offset(value: Long): OffsetType with RelativeOffset =
    implicitly[OffsetFactory[OffsetType]].offset(value)

  type EncodableCondition = (Resource)=>Boolean

  def contains(label: Label): Boolean = contains((current: Resource) => current.label == label)
  def contains(encodable: Resource): Boolean = contains((current: Resource) => current == encodable)
  def contains(condition: EncodableCondition): Boolean = content.exists(condition)

  def estimatedOffset(label: Label): Estimate[OffsetType with RelativeOffset] =
    content.takeWhile(current => current.label != label)
      .map(_.estimateSize).estimateSum.map(offset(_))

  def estimatedOffset(encodable: Resource): Estimate[OffsetType with RelativeOffset] =
    content.takeWhile(current => current != encodable)
      .map(_.estimateSize).estimateSum.map(offset(_))

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

  def offsetDirection(from: SinglePassRelativeReference[OffsetType]): OffsetDirection = {
    val firstInstruction = content.find(x => x == from || x.label.matches(from.target)).get
    if (firstInstruction.label.matches(from.target))
      if (firstInstruction==from)
        OffsetDirection.Self
      else
        OffsetDirection.Backward
    else
      OffsetDirection.Forward
  }

  @deprecated("remove this when finished reimplementing References", "recent")
  def offsetDirectionOld(from: SinglePassRelativeReference[OffsetType]): OffsetDirectionOld = {
    val firstInstruction = content.find(x => x == from || x.label.matches(from.target)).get
    if (firstInstruction.label.matches(from.target))
      if (firstInstruction==from)
        OffsetDirectionOld.None
      else
        OffsetDirectionOld.Backward
    else
      OffsetDirectionOld.Forward
  }

  private def nextContent[AddressType<:Address[OffsetType]](currentApplication: Application[OffsetType, AddressType]): List[Resource] = {
    val newContent: List[Resource] = content.map {
      case referencing: SinglePassRelativeReference[OffsetType] =>
        referencing.bind(this)

      case absolute: AbsoluteReference[OffsetType, AddressType] =>
        absolute.bind(currentApplication)

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

  def finalContent: List[Resource with Encodable]

  //assert(!finalContent.exists(r => r.estimateSize.isInstanceOf[Bounded[OffsetType]]))


  def offset[RelativeOffsetType <: OffsetType with RelativeOffset](label: Label): RelativeOffsetType =
    estimatedOffset(label).asInstanceOf[Actual[RelativeOffsetType]].value
  def offset[RelativeOffsetType <: OffsetType with RelativeOffset](encodable: Resource): RelativeOffsetType =
    estimatedOffset(encodable).asInstanceOf[Actual[RelativeOffsetType]].value

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

