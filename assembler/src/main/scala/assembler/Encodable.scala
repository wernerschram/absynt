package assembler

import assembler.sections.Section

trait Encodable {
  def label: Label

  def encodeByte()(implicit page: Section): Seq[Byte]

  def size()(implicit page: Section): Int

  lazy val labelPrefix: String =
    label match {
      case _: NoLabel => ""
      case _ => s"$label: "
    }

  override def toString: String = labelPrefix
}
