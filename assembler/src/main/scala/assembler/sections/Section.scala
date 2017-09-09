package assembler.sections

import assembler._
import assembler.reference.ReferencingInstruction

import scala.language.implicitConversions

trait Section {
  val content: List[Resource]

  val finalContent: List[Resource with Encodable] = content.map {
    case f: Resource with Encodable => f
    case r: ReferencingInstruction => r.toOnPageState()(this)
  }

  val baseAddress: Int

  type EncodableCondition = (Resource)=>Boolean

  def relativeAddress(label: Label): Int = relativeAddress((current: Resource) => current.label == label)
  def relativeAddress(encodable: Resource): Int = relativeAddress((current: Resource) => current == encodable)
  def relativeAddress(condition: EncodableCondition): Int =
  content.takeWhile(current => !condition(current)).map {
    case f: Resource with Encodable => f
    case r: ReferencingInstruction => r.toOnPageState()(this)
  }.map(current => current.size).sum
//    finalContent.takeWhile(current => !condition(current)).map(current => current.size).sum

  def contains(label: Label): Boolean = contains((current: Resource) => current.label == label)
  def contains(encodable: Resource): Boolean = contains((current: Resource) => current == encodable)
  def contains(condition: EncodableCondition): Boolean = content.exists(condition)

  def intermediateEncodables(from: ReferencingInstruction): List[Resource]

  def isForwardReference(from: ReferencingInstruction): Boolean

  def size: Int

  def encodeByte(): List[Byte]
}

class SimpleSection(val content: List[Resource], val baseAddress: Int)(implicit val label: Label) extends Section {

  def intermediateEncodables(from: ReferencingInstruction): List[Resource] = {
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

  override def isForwardReference(from: ReferencingInstruction): Boolean = {
    val firstInstruction = content.find(x => x == from || x.label.matches(from.target)).get
    !firstInstruction.label.matches(from.target)
  }

  lazy val encodeByte: List[Byte] = finalContent.flatMap { x => x.encodeByte }

  lazy val size: Int = encodeByte.length
}

object Section {
  def apply(content: List[Resource], sectionBaseAddress: Int)(implicit label: Label): Section = new SimpleSection(content, sectionBaseAddress)
}
