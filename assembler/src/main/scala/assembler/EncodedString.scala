package assembler

import assembler.memory.MemoryPage

trait EncodedString extends Encodable {
  val string: String

  def encodeByte()(implicit page: MemoryPage) = string.getBytes.toList

  def size()(implicit page: MemoryPage) = string.length()

  def withLabel(label: Label): LabeledEncodable = new LabeledEncodedString(this, label)
}

object EncodedString {
  def apply(stringValue: String) = new EncodedString { val string = stringValue }
}

class LabeledEncodedString(override val value: EncodedString, override val label: Label) extends EncodedString with LabeledEncodable {
  val string = value.string
}
