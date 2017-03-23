package assembler.sections

import assembler.Encodable
import assembler.Label

trait Section extends Encodable {
  val content: List[Encodable]
  def getRelativeAddress(encodable: Encodable): Int =
    content.takeWhile(current => current != encodable).map(current => current.size()(this)).sum

  def intermediateEncodables(from: Encodable, to: Label): List[Encodable]

  def isForwardReference(from: Encodable, to: Label): Boolean

  def size: Int
  def size()(implicit section: Section): Int = size

  def encodeByte: List[Byte]
  override def encodeByte()(implicit section: Section) = encodeByte
}

class SimpleSection(val content: List[Encodable]) extends Section {
  private def encodableLocation(encodable: Encodable): Int = content.indexOf(encodable)

  // TODO reimplement this
  private def getEncodableByLabel(label: Label): Encodable =
    content.head

  def intermediateEncodables(from: Int, to: Int): List[Encodable] =
    if (from < to) {
      content.slice(from + 1, to)
    } else {
      content.slice(to, from)
    }

  def intermediateEncodables(from: Encodable, to: Label): List[Encodable] =
    intermediateEncodables(encodableLocation(from), encodableLocation(getEncodableByLabel(to)))

  override def isForwardReference(from: Encodable, to: Label): Boolean =
    encodableLocation(from) < encodableLocation(getEncodableByLabel(to))

  lazy val encodeByte: List[Byte] = content.flatMap { x => x.encodeByte()(this) }

  lazy val size: Int = encodeByte.length
}

trait BaseAddress {
  self: Section =>

  val baseAddress: Int
}

object Section {
  def apply(content: List[Encodable]): Section = new SimpleSection(content)
  def apply(content: List[Encodable], sectionBaseAddress: Int): Section = new SimpleSection(content) with BaseAddress {
    val baseAddress: Int = sectionBaseAddress
  }
}
