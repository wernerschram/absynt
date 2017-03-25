package assembler.sections

import assembler._

import scala.language.implicitConversions

trait Section extends Encodable {
  val content: List[Designation[Encodable]]
  def getRelativeAddress(encodable: Encodable): Int =
    content.takeWhile(current => current.target != encodable).map(current => current.target.size()(this)).sum

  def intermediateEncodables(from: Encodable, to: Label): List[Encodable]

  def isForwardReference(from: Encodable, to: Label): Boolean

  def size: Int
  def size()(implicit section: Section): Int = size

  def encodeByte: List[Byte]
  override def encodeByte()(implicit section: Section) = encodeByte
}

class SimpleSection(val content: List[Designation[Encodable]]) extends Section {
  def intermediateEncodables(from: Encodable, to: Label): List[Encodable] = {
    val trimLeft = content
      .dropWhile(x => !(x.target == from || (x.isLabeled && x.label == to)))

    val trimRight = trimLeft.tail
      .takeWhile(x => !(x.target == from || (x.isLabeled && x.label == to)))

    if (trimLeft.head.target == from)
      trimRight.map { x => x.target }
    else
      trimLeft.head.target :: trimRight.map { x => x.target }
  }

  override def isForwardReference(from: Encodable, to: Label): Boolean =
    content.find(x => x.target == from || (x.isLabeled && x.label == to)).get.target == from

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
