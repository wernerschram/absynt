package assembler

import assembler.resource.UnlabeledEncodable

case class EncodedString(string: String)(implicit label: Label) extends UnlabeledEncodable {
  def encodeByte: Seq[Byte] = string.getBytes.toList

  def size: Int = string.length()

  override def toString: String = s"""SETS "$string""""
}
