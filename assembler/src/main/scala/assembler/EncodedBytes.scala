package assembler

import assembler.ListExtensions._
import assembler.resource.Encodable

case class EncodedBytes(bytes: Seq[Byte])(implicit label: Label) extends Encodable {

  def encodeByte: Seq[Byte] = bytes

  def size: Int = bytes.length

  override def toString: String = s"""SETB "${bytes.bigEndianHexString}""""
}

object EncodedBytes {
  def apply(byte: Byte)(implicit label: Label): EncodedBytes = new EncodedBytes(Seq(byte))
}
