package assembler

trait Encodable {
  
  def encodeByte()(implicit page: MemoryPage): List[Byte]
  
  def size()(implicit page: MemoryPage): Int
  
  def withLabel(label: Label): LabeledEncodable
}

trait LabeledEncodable extends Encodable with Labeled {
}