package assembler

trait Resource {
  def label: Label

  lazy val labelPrefix: String =
    label match {
      case _: NoLabel => ""
      case _ => s"$label: "
    }

  override def toString: String = labelPrefix
}

trait Encodable extends Resource {
  self: Resource =>
  def encodeByte: Seq[Byte]

  def size: Int

}
