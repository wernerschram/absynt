package assembler.sections

import assembler.Encodable
import assembler.Label
import assembler.LabeledEncodable

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

  override def withLabel(label: Label) = new LabeledSection(this, label)

}

class LabeledSection(override val value: Section, override val label: Label) extends Section with LabeledEncodable {
  override def size = value.size

  override def isForwardReference(from: Encodable, to: Label) = value.isForwardReference(from, to)

  override val content = value.content

  override def encodeByte = value.encodeByte

  override def getRelativeAddress(encodable: Encodable): Int = value.getRelativeAddress(encodable)

  override def intermediateEncodables(from: Encodable, to: Label) = value.intermediateEncodables(from, to)
}

class SimpleSection(val content: List[Encodable]) extends Section {
  private def encodableLocation(encodable: Encodable): Int = content.indexOf(encodable)

  private def getEncodableByLabel(label: Label): Encodable =
    content.filter {
      case encodable: LabeledEncodable => encodable.label == label
      case _ => false
    }.head

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
