package assembler

import assembler.memory.MemoryPage

trait EncodedString extends Encodable {
  val string: String

  def encodeByte()(implicit page: MemoryPage): List[Byte] = string.getBytes.toList

  def size()(implicit page: MemoryPage): Int = string.length()

  def withLabel(label: Label): LabeledEncodable = new LabeledEncodedString(this, label)
}

object EncodedString {
  def apply(stringValue: String) = new EncodedString { val string: String = stringValue }
}

class LabeledEncodedString(override val value: EncodedString, override val label: Label) extends EncodedString with LabeledEncodable {
  val string: String = value.string
}