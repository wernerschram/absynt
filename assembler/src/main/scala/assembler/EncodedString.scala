package assembler

import assembler.resource.Encodable

class EncodedString private(val string: String)(override val label: Label) extends Encodable {
  def encodeByte: List[Byte] = string.getBytes.toList

  def size: Int = string.length()

  override def toString: String = s"""${labelPrefix}SETS "$string""""
}

object EncodedString {
  def apply(string: String)(implicit label: Label) = new EncodedString(string)(label)
}
