package assembler

import assembler.ListExtensions._

trait EncodedByteList extends Resource with Encodable {
  val bytes: List[Byte]

  def encodeByte: List[Byte] = bytes

  def size: Int = bytes.length

  override def toString: String = s"""${labelPrefix}SETB "${bytes.bigEndianHexString}""""
}

object EncodedByteList {
  def apply(bytesValue: List[Byte])(implicit newLabel: Label) = new EncodedByteList { val label: Label = newLabel; val bytes: List[Byte] =
    bytesValue }
}

