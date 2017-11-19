package assembler.x86.operands.memoryaccess

import assembler.{Address, OffsetFactory}
import assembler.ListExtensions._
import assembler.x86.ProcessorMode
import assembler.x86.operands.{FarPointerSize, FixedSizeOperand, Operand}

sealed abstract case class FarPointer[OffsetType <: X86Offset](segment: Short, offset: OffsetType)
  extends Address[OffsetType] with Operand with FixedSizeOperand {

  override val operandByteSize: FarPointerSize
  override def toString =
    s"FAR 0x${segment.encodeLittleEndian.bigEndianHexString}:0x${offset.encode(1).bigEndianHexString}"

  def encodeByte: List[Byte] = offset.encode(1) ::: segment.encodeLittleEndian
}

object FarPointer {
  def apply(segment: Short, offset: RealX86Offset): FarPointer[RealX86Offset] =
    new FarPointer[RealX86Offset](segment, offset) {
      implicit def OffsetFactory: OffsetFactory[RealX86Offset] = ProcessorMode.Real.offsetFactory

      override val operandByteSize: FarPointerSize = FarPointerSize.DoubleWord
      override def toLong: Long = offset.offset
    }

  def apply(segment: Short, offset: ProtectedX86Offset): FarPointer[ProtectedX86Offset] =
    new FarPointer[ProtectedX86Offset](segment, offset) {
      implicit def OffsetFactory: OffsetFactory[ProtectedX86Offset] = ProcessorMode.Protected.offsetFactory

      override val operandByteSize: FarPointerSize = FarPointerSize.FarWord
      override def toLong: Long = offset.offset
    }
}
