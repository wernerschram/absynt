package assembler

import assembler.sections.Section

trait Encodable {
  def label: Label

  def encodeByte()(implicit page: Section): List[Byte]

  def size()(implicit page: Section): Int

  override def toString: String =
    label match {
      case _: NoLabel => ""
      case _ => s"$label: "
    }
}
