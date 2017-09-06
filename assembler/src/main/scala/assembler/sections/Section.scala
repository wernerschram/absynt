package assembler.sections

import assembler._
import assembler.reference.ReferencingInstruction

import scala.language.implicitConversions

trait Section {
  val content: List[Encodable]

  val baseAddress: Int

  type EncodableCondition = (Encodable)=>Boolean

  def relativeAddress(label: Label): Int = relativeAddress((current: Encodable) => current.label == label)
  def relativeAddress(encodable: Encodable): Int = relativeAddress((current: Encodable) => current == encodable)
  def relativeAddress(condition: EncodableCondition): Int =
    content.takeWhile(current => !condition(current)).map(current => current.size()(this)).sum

  def contains(label: Label): Boolean = contains((current: Encodable) => current.label == label)
  def contains(encodable: Encodable): Boolean = contains((current: Encodable) => current == encodable)
  def contains(condition: EncodableCondition): Boolean = content.exists(condition)

  def intermediateEncodables(from: ReferencingInstruction): List[Encodable]

  def isForwardReference(from: ReferencingInstruction): Boolean

  def size: Int

  def encodeByte(): List[Byte]
}

class SimpleSection(val content: List[Encodable], val baseAddress: Int)(implicit val label: Label) extends Section {

  def intermediateEncodables(from: ReferencingInstruction): List[Encodable] = {
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

  lazy val encodeByte: List[Byte] = content.flatMap { x => x.encodeByte()(this) }

  lazy val size: Int = encodeByte.length
}

object Section {
  def apply(content: List[Encodable], sectionBaseAddress: Int)(implicit label: Label): Section = new SimpleSection(content, sectionBaseAddress)
}
