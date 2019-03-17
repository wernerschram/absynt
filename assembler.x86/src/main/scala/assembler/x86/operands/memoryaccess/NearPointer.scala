package assembler.x86.operands.memoryaccess

import assembler.ListExtensions._
import assembler.x86.operands._

sealed abstract class NearPointer(val offset: Seq[Byte])
  extends Operand {
  self: ValueSize =>

  def encodeBytes: Seq[Byte] = offset
}

object ShortPointer {
  def apply(offset: Byte): NearPointer with ByteSize =
    new NearPointer(offset.encodeLittleEndian) with ByteSize {
      override def toString: String = s"0x${offset.bigEndianHexString}"
    }
}

object LongPointer {
  def realMode(offset: Int): NearPointer with WordSize = {
    assume(offset.toShort == offset)
    new NearPointer(offset.toShort.encodeLittleEndian) with WordSize {
      override def toString: String = s"0x${offset.bigEndianHexString}"
    }
  }

  def protectedMode(offset: Int): NearPointer with DoubleWordSize =
     new NearPointer(offset.encodeLittleEndian) with DoubleWordSize {
       override def toString: String = s"0x${offset.bigEndianHexString}"
    }

}