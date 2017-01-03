package assembler

import assembler.memory.MemoryPage

trait Encodable {
  def encodeByte()(implicit page: MemoryPage): List[Byte]

  def size()(implicit page: MemoryPage): Int

  def withLabel(label: Label): LabeledEncodable
}

trait LabeledEncodable extends Encodable with Labeled {
  val value: Encodable
  
  override def size()(implicit page: MemoryPage): Int = value.size()
  override def encodeByte()(implicit page: MemoryPage): List[Byte] = value.encodeByte()

  override def toString = s"$label: $value"
}
