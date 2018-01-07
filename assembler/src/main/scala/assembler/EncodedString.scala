package assembler

import assembler.resource.Encodable

case class EncodedString(string: String)(implicit label: Label) extends Encodable(label) {
  def encodeByte: Seq[Byte] = string.getBytes.toList

  def size: Int = string.length()

  override def toString: String = s"""${labelPrefix}SETS "$string""""
}
