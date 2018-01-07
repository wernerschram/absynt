package assembler.x86.operands.memoryaccess

import assembler.ListExtensions._
import assembler.RelativeOffset
import assembler.x86.operands.{FixedSizeOperand, Operand, OperandSize, ValueSize}

sealed abstract class NearPointer[OffsetType <: X86Offset](val offset: OffsetType with RelativeOffset)
  extends Operand with FixedSizeOperand {
  val operandByteSize: OperandSize

  def encodeBytes: Seq[Byte]
}

object ShortPointer {
  def apply[OffsetType <: X86Offset](offset: OffsetType with RelativeOffset): NearPointer[OffsetType] =
    new NearPointer[OffsetType](offset) {
      val operandByteSize: ValueSize = ValueSize.Byte

      override def encodeBytes: Seq[Byte] = offset.encodeShort(1)

      override def toString: String = s"0x${offset.encodeShort(1).bigEndianHexString}"
    }
}

object LongPointer {
  def apply[OffsetType <: X86Offset](offset: OffsetType with RelativeOffset): NearPointer[OffsetType] =
    new NearPointer[OffsetType](offset) {
      val operandByteSize: OperandSize = offset.operandByteSize

      override def encodeBytes: Seq[Byte] = offset.encode(1)

      override def toString: String = s"0x${offset.encode(1).bigEndianHexString}"
    }
}