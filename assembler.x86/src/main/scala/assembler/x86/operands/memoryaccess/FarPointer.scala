package assembler.x86.operands.memoryaccess

import assembler.ListExtensions._
import assembler.x86.operands.{FarPointerSize, FixedSizeOperand, Operand}

sealed abstract case class FarPointer[OffsetType <: X86Offset](segment: Short, offset: OffsetType)
  extends Operand with FixedSizeOperand {

  override val operandByteSize: FarPointerSize
  override def toString =
    s"FAR 0x${segment.encodeLittleEndian.bigEndianHexString}:0x${offset.encode(1).bigEndianHexString}"

  def encodeByte: Seq[Byte] = offset.encode(1) ++ segment.encodeLittleEndian
}

object FarPointer {
  def apply(segment: Short, offset: RealX86Offset): FarPointer[RealX86Offset] =
    new FarPointer[RealX86Offset](segment, offset) {
      override val operandByteSize: FarPointerSize = FarPointerSize.DoubleWord
    }

  def apply(segment: Short, offset: ProtectedX86Offset): FarPointer[ProtectedX86Offset] =
    new FarPointer[ProtectedX86Offset](segment, offset) {
      override val operandByteSize: FarPointerSize = FarPointerSize.FarWord
    }
}
