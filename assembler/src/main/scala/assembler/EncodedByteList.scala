package assembler

import assembler.sections.Section
import assembler.ListExtensions._

trait EncodedByteList extends Encodable {
  val bytes: List[Byte]

  def encodeByte()(implicit page: Section): List[Byte] = bytes

  def size()(implicit page: Section): Int = bytes.length

  override def toString: String = s"""${super.toString}SETB "${bytes.bigEndianHexString}""""
}

object EncodedByteList {
  def apply(bytesValue: List[Byte])(implicit newLabel: Label) = new EncodedByteList { val label: Label = newLabel; val bytes: List[Byte] =
    bytesValue }
}

