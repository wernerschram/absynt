package assembler

import assembler.sections.Section

class EncodedString private(val string: String)(override val label: Label) extends Encodable {
  def encodeByte()(implicit page: Section): List[Byte] = string.getBytes.toList

  def size()(implicit page: Section): Int = string.length()

  override def toString: String = s"""${super.toString}SETS "$string""""
}

object EncodedString {
  def apply(string: String)(implicit label: Label) = new EncodedString(string)(label)
}
