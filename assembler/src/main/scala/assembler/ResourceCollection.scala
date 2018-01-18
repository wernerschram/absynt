package assembler

import assembler.resource.{Encodable, Resource}

case class ResourceCollection(encodables: Seq[Resource with Encodable])(implicit override val l: Label) extends Encodable(l) {

  override def encodeByte: Seq[Byte] = encodables.flatMap(_.encodeByte)

  override def size: Int = encodables.map(_.size).sum

  override def toString: String = s"""$labelPrefix${encodables.map(_.toString).mkString("; ")}"""
}
