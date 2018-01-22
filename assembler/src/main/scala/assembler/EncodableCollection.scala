package assembler

import assembler.resource.UnlabeledEncodable

case class EncodableCollection(encodables: Seq[UnlabeledEncodable]) extends UnlabeledEncodable {

  override def encodeByte: Seq[Byte] = encodables.flatMap(_.encodeByte)

  override def size: Int = encodables.map(_.size).sum

  override def toString: String = s"""${encodables.map(_.toString).mkString("; ")}"""
}
