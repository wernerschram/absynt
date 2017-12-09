package assembler

import assembler.ListExtensions._

trait EncodedByteList extends Encodable {
  val bytes: Seq[Byte]

  def encodeByte: Seq[Byte] = bytes

  def size: Int = bytes.length

  override def toString: String = s"""${labelPrefix}SETB "${bytes.bigEndianHexString}""""
}

object EncodedByteList {
  def apply(bytesValue: Seq[Byte])(implicit newLabel: Label) = new EncodedByteList { val label: Label = newLabel; val bytes: Seq[Byte] =
    bytesValue }
}

