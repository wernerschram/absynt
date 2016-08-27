package assembler

trait Encodable[ResultType <: AnyVal] {
  
  def encode()(implicit page: MemoryPage): List[ResultType]

  def encodeByte()(implicit page: MemoryPage): List[Byte]
  
  def size()(implicit page: MemoryPage): Int
}

trait LabeledEncodable[ResultType <: AnyVal] extends Encodable[ResultType] with Labeled {
}