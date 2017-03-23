package assembler

import assembler.sections.Section

trait Encodable {
  def encodeByte()(implicit page: Section): List[Byte]

  def size()(implicit page: Section): Int
}
