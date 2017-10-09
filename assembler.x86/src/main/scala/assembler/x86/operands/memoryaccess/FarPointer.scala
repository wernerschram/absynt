package assembler.x86.operands.memoryaccess

import assembler.Address
import assembler.ListExtensions._
import assembler.x86.OffsetFactory
import assembler.x86.operands.{FarPointerSize, FixedSizeOperand, Operand}

sealed abstract class FarPointer[OffsetType <: X86Offset](val segment: Short, val offset: OffsetType)
  extends Address[OffsetType] with Operand with FixedSizeOperand {

  override val operandByteSize: FarPointerSize
  override def toString = s"FAR 0x${segment.encodeLittleEndian.bigEndianHexString}:0x${offset.encode.bigEndianHexString}"

  def encodeByte: List[Byte] = offset.encode ::: segment.encodeLittleEndian

  override def add(that: OffsetType): FarPointer[OffsetType]
  override def +(that: OffsetType): FarPointer[OffsetType] = this.add(that)
}

object FarPointer {
  def apply(segment: Short, offset: RealOffset): FarPointer[RealOffset] =
    new FarPointer[RealOffset](segment, offset) {
       implicit def OffsetFactory: OffsetFactory[RealOffset] = (offsetValue: Long) => new RealOffset(offsetValue)

      override val operandByteSize: FarPointerSize = FarPointerSize.DoubleWord
      override def add(that: RealOffset): FarPointer[RealOffset] = apply(segment, offset + that)
    }

  def apply(segment: Short, offset: ProtectedOffset): FarPointer[ProtectedOffset] =
    new FarPointer[ProtectedOffset](segment, offset) {
      implicit def OffsetFactory: OffsetFactory[ProtectedOffset] = (offsetValue: Long) => new ProtectedOffset(offsetValue)

      override val operandByteSize: FarPointerSize = FarPointerSize.FarWord
      override def add(that: ProtectedOffset): FarPointer[ProtectedOffset] = apply(segment, offset + that)
    }
}
