package assembler

import assembler.sections.Section

trait Encodable {
  def encodeByte()(implicit page: Section): List[Byte]

  def size()(implicit page: Section): Int

  def withLabel(label: Label): LabeledEncodable
}

trait LabeledEncodable extends Encodable with Labeled {
  val value: Encodable
  
  override def size()(implicit page: Section): Int = value.size()
  override def encodeByte()(implicit page: Section): List[Byte] = value.encodeByte()

  override def toString = s"$label: $value"
}
