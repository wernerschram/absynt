package assembler.x86.operands.memoryaccess

import assembler.ListExtensions._
import assembler.x86.operands._

sealed abstract class NearPointer(val offset: Seq[Byte])
  extends Operand with ValueSize {

  def encodeBytes: Seq[Byte] = offset
}

object ShortPointer {
  def apply(offset: Byte): NearPointer with ByteSize =
    new NearPointer(offset.encodeLittleEndian) with ByteSize {
      override def toString: String = s"0x${offset.bigEndianHexString}"
    }

  def apply(offset: Long): NearPointer with ByteSize = {
    assume(offset.toByte == offset)
    apply(offset.toByte)
  }

}

object LongPointer {
  def realMode(offset: Short): NearPointer with WordSize =
     new NearPointer(offset.encodeLittleEndian) with WordSize {
       override def toString: String = s"0x${offset.bigEndianHexString}"
    }

  def realMode(offset: Long): NearPointer with WordSize = {
    assume(offset.toShort == offset)
    realMode(offset.toShort)
  }

  def protectedMode(offset: Int): NearPointer with DoubleWordSize =
     new NearPointer(offset.encodeLittleEndian) with DoubleWordSize {
       override def toString: String = s"0x${offset.bigEndianHexString}"
    }

}