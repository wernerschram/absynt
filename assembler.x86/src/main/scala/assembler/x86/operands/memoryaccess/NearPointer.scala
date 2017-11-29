package assembler.x86.operands.memoryaccess

import assembler.ListExtensions._
import assembler.x86.X86OffsetFactory
import assembler.x86.operands.{FixedSizeOperand, Operand, OperandSize, ValueSize}
import assembler.{Address, OffsetFactory, RelativeOffset}

sealed abstract class NearPointer[OffsetType <: X86Offset : OffsetFactory](val offset: OffsetType with RelativeOffset)
  extends Address[OffsetType] with Operand with FixedSizeOperand {
  val operandByteSize: OperandSize

  def encodeBytes: List[Byte]
}

object ShortPointer {
  def apply[OffsetType <: X86Offset : X86OffsetFactory](offset: OffsetType with RelativeOffset): NearPointer[OffsetType] =
    new NearPointer[OffsetType](offset) {
      val operandByteSize: ValueSize = ValueSize.Byte

      override def encodeBytes: List[Byte] = offset.encodeShort(1)

      override def toString: String = s"0x${offset.encodeShort(1).bigEndianHexString}"
    }
}

object LongPointer {
  def apply[OffsetType <: X86Offset : X86OffsetFactory](offset: OffsetType with RelativeOffset): NearPointer[OffsetType] =
    new NearPointer[OffsetType](offset) {
      val operandByteSize: OperandSize = offset.operandByteSize

      override def encodeBytes: List[Byte] = offset.encode(1)

      override def toString: String = s"0x${offset.encode(1).bigEndianHexString}"
    }
}