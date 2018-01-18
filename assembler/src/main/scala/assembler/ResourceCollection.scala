package assembler

import assembler.resource.{Encodable, Resource}

case class ResourceCollection(encodables: Seq[Resource with Encodable]) extends Encodable {

  override def encodeByte: Seq[Byte] = encodables.flatMap(_.encodeByte)

  override def size: Int = encodables.map(_.size).sum

  override def toString: String = s"""${encodables.map(_.toString).mkString("; ")}"""
}
