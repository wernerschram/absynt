package assembler.sections

import assembler.Encodable
import assembler.Label
import assembler.LabeledEncodable

trait Section {
  val content: Seq[Encodable]
  def getRelativeAddress(encodable: Encodable): Int =
    content.takeWhile(current => current != encodable).map(current => current.size()(this)).sum

  def intermediateEncodables(from: Encodable, to: Label): Seq[Encodable]

  def isForwardReference(from: Encodable, to: Label): Boolean

  def encodeByte(): Seq[Byte]

  def size: Int
}

class SimpleSection(val content: Seq[Encodable]) extends Section {
  private def encodableLocation(encodable: Encodable): Int = content.indexOf(encodable)

  private def getEncodableByLabel(label: Label): Encodable =
    content.filter {
      case encodable: LabeledEncodable => encodable.label == label
      case _ => false
    }.head

  def intermediateEncodables(from: Int, to: Int): Seq[Encodable] =
    if (from < to) {
      content.slice(from + 1, to)
    } else {
      content.slice(to, from)
    }

  def intermediateEncodables(from: Encodable, to: Label): Seq[Encodable] =
    intermediateEncodables(encodableLocation(from), encodableLocation(getEncodableByLabel(to)))

  override def isForwardReference(from: Encodable, to: Label): Boolean =
    encodableLocation(from) < encodableLocation(getEncodableByLabel(to))

  lazy val encodeByte: Seq[Byte] = content.flatMap { x => x.encodeByte()(this) }

  lazy val size: Int = encodeByte.length
}

trait BaseAddress {
  self: Section =>

  val baseAddress: Int
}

object Section {
  def apply(content: Seq[Encodable]): Section = new SimpleSection(content)
  def apply(content: Seq[Encodable], sectionBaseAddress: Int): Section = new SimpleSection(content) with BaseAddress {
    val baseAddress: Int = sectionBaseAddress
  }
}
