package assembler.sections

import assembler.Encodable
import assembler.Label
import assembler.LabeledEncodable

trait Section {
  val content: Seq[Encodable]
  def intermediateEncodables(from: Encodable, to: Label): Seq[Encodable]

  def isForwardReference(from: Encodable, to: Label): Boolean

  def encodeByte(): Seq[Byte]
}

class SimpleSection(val content: Seq[Encodable]) extends Section {
  private def encodableLocation(encodable: Encodable): Int = content.indexOf(encodable)

  private def getEncodableByLabel(label: Label): Encodable =
    content.filter {
      case encodable: LabeledEncodable => encodable.label == label
      case _ => false
    }.head

  private def intermediateEncodables(from: Int, to: Int): Seq[Encodable] =
    if (from < to) {
      content.slice(from + 1, to)
    } else {
      content.slice(to, from)
    }

  def intermediateEncodables(from: Encodable, to: Label): Seq[Encodable] =
    intermediateEncodables(encodableLocation(from), encodableLocation(getEncodableByLabel(to)))

  def isForwardReference(from: Encodable, to: Label): Boolean =
    encodableLocation(from) < encodableLocation(getEncodableByLabel(to))

  def encodeByte(): Seq[Byte] = content.flatMap { x => x.encodeByte()(this) }
}

trait SectionLocation {
  self: Section =>

}

object Section {
  def apply(content: Seq[Encodable]) = new SimpleSection(content)
}