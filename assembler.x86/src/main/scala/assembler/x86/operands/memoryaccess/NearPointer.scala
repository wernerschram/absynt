package assembler.x86.operands.memoryaccess

import assembler.ListExtensions._
import assembler.x86.operands._

sealed abstract class NearPointer(val offset: Seq[Byte])
  extends Operand with FixedSizeOperand {

  def encodeBytes: Seq[Byte] = offset
}

object ShortPointer {
  def apply(offset: Byte): NearPointer =
    new NearPointer(offset.encodeLittleEndian) {
      override def toString: String = s"0x${offset.bigEndianHexString}"

      override def operandByteSize: OperandSize = ValueSize.Byte
    }

  def apply(offset: Long): NearPointer = {
    assume(offset.toByte == offset)
    apply(offset.toByte)
  }

}

object LongPointer {
  def realMode(offset: Short): NearPointer =
     new NearPointer(offset.encodeLittleEndian) {
       override def toString: String = s"0x${offset.bigEndianHexString}"

       override def operandByteSize: OperandSize = ValueSize.Word
    }

  def realMode(offset: Long): NearPointer = {
    assume(offset.toShort == offset)
    realMode(offset.toShort)
  }

  def protectedMode(offset: Int): NearPointer =
     new NearPointer(offset.encodeLittleEndian) {
       override def toString: String = s"0x${offset.bigEndianHexString}"

       override def operandByteSize: OperandSize = ValueSize.DoubleWord
    }

}