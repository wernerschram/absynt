package assembler

import assembler.memory.MemoryPage

trait EncodedByteList extends Encodable {
  val bytes: List[Byte]

  def encodeByte()(implicit page: MemoryPage) = bytes

  def size()(implicit page: MemoryPage) = bytes.length

  def withLabel(label: Label): LabeledEncodable = new LabeledEncodedByteList(this, label)
}

object EncodedByteList {
  def apply(bytesValue: List[Byte]) = new EncodedByteList { val bytes = bytesValue }
}

class LabeledEncodedByteList(override val value: EncodedByteList, override val label: Label) extends EncodedByteList with LabeledEncodable {
  override val bytes = value.bytes
}
