package assembler.x86.operands.memoryaccess

import assembler.Address
import assembler.ListExtensions._
import assembler.x86.operands.{FarPointerSize, FixedSizeOperand, Operand}

sealed abstract class FarPointer[OffsetType: Numeric](val segment: Short, val offset: OffsetType)
  extends Address[OffsetType] with Operand with FixedSizeOperand {
  def encodeOffset: List[Byte]

  override val operandByteSize: FarPointerSize
  override def toString = s"FAR 0x${segment.encodeLittleEndian.bigEndianHexString}:0x${encodeOffset.bigEndianHexString}"

  def encodeByte: List[Byte] = encodeOffset ::: segment.encodeLittleEndian

  override def add(that: OffsetType): FarPointer[OffsetType] =
    new FarPointer[OffsetType](segment, implicitly[Numeric[OffsetType]].plus(that, offset)) {
      override def encodeOffset: List[Byte] = FarPointer.this.encodeOffset

      override val operandByteSize: FarPointerSize = FarPointer.this.operandByteSize
    }
}

object FarPointer {
  def apply(segment: Short, offset: X86Offset.RealLongOffset): FarPointer[X86Offset.RealLongOffset] = new FarPointer[X86Offset.RealLongOffset](segment, offset) {
    override val operandByteSize: FarPointerSize = FarPointerSize.DoubleWord

    override def encodeOffset: List[Byte] = offset.encodeLittleEndian
  }

  def apply(segment: Short, offset: X86Offset.ProtectedLongOffset): FarPointer[X86Offset.ProtectedLongOffset] = new FarPointer[X86Offset.ProtectedLongOffset](segment, offset) {
    override val operandByteSize: FarPointerSize = FarPointerSize.FarWord

    override def encodeOffset: List[Byte] = offset.encodeLittleEndian
  }
}