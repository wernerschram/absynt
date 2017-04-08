package assembler

import assembler.sections.Section

trait EncodedString extends Encodable {
  val string: String

  def encodeByte()(implicit page: Section): List[Byte] = string.getBytes.toList

  def size()(implicit page: Section): Int = string.length()
}

object EncodedString {
  def apply(stringValue: String)(implicit newLabel: Label) = new EncodedString { val label = newLabel; val string: String = stringValue }
}
