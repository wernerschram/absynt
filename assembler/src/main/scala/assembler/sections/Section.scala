package assembler.sections

import assembler._
import assembler.reference.ReferencingInstruction

import scala.language.implicitConversions

trait Section extends Encodable {
  val content: List[Encodable]
  def getRelativeAddress(encodable: Encodable): Int =
    content.takeWhile(current => current != encodable).map(current => current.size()(this)).sum

  def intermediateEncodables(from: ReferencingInstruction): List[Encodable]

  def isForwardReference(from: ReferencingInstruction): Boolean

  def size: Int
  def size()(implicit section: Section): Int = size

  def encodeByte: List[Byte]
  override def encodeByte()(implicit section: Section): List[Byte] = encodeByte
}

class SimpleSection(val content: List[Encodable])(implicit val label: Label) extends Section {
  def intermediateEncodables(from: ReferencingInstruction): List[Encodable] = {
    val trimLeft = content
      .dropWhile(x => !(x == from || x.label.matches(from.target)))

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

trait BaseAddress {
  self: Section =>

  val baseAddress: Int
}

object Section {
  def apply(content: List[Encodable])(implicit label: Label): Section = new SimpleSection(content)
  def apply(content: List[Encodable], sectionBaseAddress: Int)(implicit label: Label): Section = new SimpleSection(content) with BaseAddress {
    val baseAddress: Int = sectionBaseAddress
  }
}
