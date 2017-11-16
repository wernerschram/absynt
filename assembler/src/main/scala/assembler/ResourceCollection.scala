package assembler

class ResourceCollection private(val label: Label, val encodables: List[Resource with Encodable]) extends Encodable {

  override def encodeByte: Seq[Byte] = encodables.flatMap(_.encodeByte)

  override def size: Int = encodables.map(_.size).sum

  override def toString: String = s"""$labelPrefix${encodables.map(_.toString).mkString("; ")}"""
}

object ResourceCollection {
  def apply(encodables: List[Resource with Encodable])(implicit label: Label): ResourceCollection = new ResourceCollection(label, encodables)
}