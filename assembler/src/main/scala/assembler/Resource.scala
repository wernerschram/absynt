package assembler

trait Resource {
  def label: Label

  def minimumSize: Int
  def maximumSize: Int

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

  override def minimumSize: Int = size
  override def maximumSize: Int = size

  def size: Int

}
