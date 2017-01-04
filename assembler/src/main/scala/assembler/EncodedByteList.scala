package assembler

import assembler.sections.Section

trait EncodedByteList extends Encodable {
  val bytes: List[Byte]

  def encodeByte()(implicit page: Section): List[Byte] = bytes

  def size()(implicit page: Section): Int = bytes.length

  def withLabel(label: Label): LabeledEncodable = new LabeledEncodedByteList(this, label)
}

object EncodedByteList {
  def apply(bytesValue: List[Byte]) = new EncodedByteList { val bytes: List[Byte] = bytesValue }
}

class LabeledEncodedByteList(override val value: EncodedByteList, override val label: Label) extends EncodedByteList with LabeledEncodable {
  override val bytes: List[Byte] = value.bytes
}
