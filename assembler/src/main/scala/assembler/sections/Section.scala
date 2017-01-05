package assembler.sections

import assembler.Encodable
import assembler.Label
import assembler.LabeledEncodable

trait Section {
  val content: Seq[Encodable]
  def encodableLocation(encodable: Encodable): Int

  def getEncodableByLabel(label: Label): Encodable
  def intermediateEncodables(from: Int, to: Int): Seq[Encodable]

  def encodeByte(): Seq[Byte]
}

class SimpleSection(val content: Seq[Encodable]) extends Section {
  def encodableLocation(encodable: Encodable): Int = content.indexOf(encodable)

  def getEncodableByLabel(label: Label): Encodable =
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

  def encodeByte(): Seq[Byte] = content.flatMap { x => x.encodeByte()(this) }
}

trait SectionLocation {
  self: Section =>

}

object Section {
  def apply(content: Seq[Encodable]) = new SimpleSection(content)
}