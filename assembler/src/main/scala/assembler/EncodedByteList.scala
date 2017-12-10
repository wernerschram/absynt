package assembler

import assembler.ListExtensions._
import assembler.resource.Encodable

case class EncodedByteList(bytes: Seq[Byte])(implicit label: Label) extends Encodable(label) {

  def encodeByte: Seq[Byte] = bytes

  def size: Int = bytes.length

  override def toString: String = s"""${labelPrefix}SETB "${bytes.bigEndianHexString}""""
}

