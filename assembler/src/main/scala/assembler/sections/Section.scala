package assembler.sections

import assembler._
import assembler.reference.ReferencingInstruction

import scala.language.implicitConversions

trait Section extends Encodable {
  val content: List[Designation[Encodable]]
  def getRelativeAddress(encodable: Encodable): Int =
    content.takeWhile(current => current.target != encodable).map(current => current.target.size()(this)).sum

  def intermediateEncodables(from: ReferencingInstruction): List[Encodable]

  def isForwardReference(from: ReferencingInstruction): Boolean

  def size: Int
  def size()(implicit section: Section): Int = size

  def encodeByte: List[Byte]
  override def encodeByte()(implicit section: Section): List[Byte] = encodeByte
}

class SimpleSection(val content: List[Designation[Encodable]]) extends Section {
  def intermediateEncodables(from: ReferencingInstruction): List[Encodable] = {
    val trimLeft = content
      .dropWhile(x => !(x.target == from || (x.isLabeled && x.label == from.target)))

    val trimRight = trimLeft.tail
      .takeWhile(x => !(x.target == from || (x.isLabeled && x.label == from.target)))

    if (trimLeft.head.target == from)
      trimRight.map { x => x.target }
    else
      trimLeft.head.target :: trimRight.map { x => x.target }
  }

  override def isForwardReference(from: ReferencingInstruction): Boolean = {
    val firstInstruction = content.find(x => x.target == from || (x.isLabeled && x.label == from.target)).get
    !(firstInstruction.isLabeled && firstInstruction.label == from.target)
  }

  lazy val encodeByte: List[Byte] = content.flatMap { x => x.target.encodeByte()(this) }

  lazy val size: Int = encodeByte.length
}

trait BaseAddress {
  self: Section =>

  val baseAddress: Int
}

object Section {
  def apply(content: List[Designation[Encodable]]): Section = new SimpleSection(content)
  def apply(content: List[Designation[Encodable]], sectionBaseAddress: Int): Section = new SimpleSection(content) with BaseAddress {
    val baseAddress: Int = sectionBaseAddress
  }
}
