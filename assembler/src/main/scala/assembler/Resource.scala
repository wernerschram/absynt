package assembler

trait Resource {
  def label: Label

  def estimateSize: Estimate[Int]

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

  override def estimateSize: Estimate[Int] = Actual(size)

  def size: Int

}
