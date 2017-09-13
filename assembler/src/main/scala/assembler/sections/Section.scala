package assembler.sections

import assembler._
import assembler.reference.{AbsoluteReference, RelativeReference, RelativeReferenceInSection}

import scala.annotation.tailrec
import scala.language.implicitConversions

trait Section {
  val content: List[Resource]

  val name=".text"

  val baseAddress: Int

  type EncodableCondition = (Resource)=>Boolean

  def contains(label: Label): Boolean = contains((current: Resource) => current.label == label)
  def contains(encodable: Resource): Boolean = contains((current: Resource) => current == encodable)
  def contains(condition: EncodableCondition): Boolean = content.exists(condition)

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
  def apply(resources: List[Resource], base: Int): Section =
    new Section {
      override val content: List[Resource] = resources
      override val baseAddress: Int = base
    }

  private def nextContent(previousSection: Section): List[Resource] = {

    val newContent: List[Resource] = previousSection.content.map {
      case referencing: RelativeReference => referencing.toOnPageState(previousSection)
      case absolute: AbsoluteReference => absolute.toInSectionState(previousSection)
      case resource: Resource => resource
    }
    newContent
  }

  @tailrec
  def encodable(section: Section): Section with LastIteration = {
    val newContent = nextContent(section)
    if (newContent.forall { case _: Encodable => true; case _ => false }) {
      new Section with LastIteration {
        override val finalContent: List[Resource with Encodable] = newContent.map(r => r.asInstanceOf[Resource with Encodable])
        override val content: List[Resource] = finalContent
        override val baseAddress: Int = section.baseAddress
      }
    } else {
      encodable(new Section {
        override val content: List[Resource] = newContent
        override val baseAddress: Int = section.baseAddress
      })
    }
  }
}

