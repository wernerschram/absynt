package assembler.x86.operands.memoryaccess

import assembler.ListExtensions._
import assembler.x86.operands.{FarPointerSize, FixedSizeOperand, Operand}

sealed abstract case class FarPointer(segment: Short, offset: Seq[Byte])
  extends Operand with FixedSizeOperand {

  override val operandByteSize: FarPointerSize
  override def toString =
    s"FAR 0x${segment.encodeLittleEndian.bigEndianHexString}:0x${offset.bigEndianHexString}"

  def encodeByte: Seq[Byte] = offset ++ segment.encodeLittleEndian
}

object FarPointer {
  def apply(segment: Short, offset: Short): FarPointer =
    new FarPointer(segment, offset.encodeLittleEndian) {
      override val operandByteSize: FarPointerSize = FarPointerSize.DoubleWord
    }

  def apply(segment: Short, offset: Int): FarPointer =
    new FarPointer(segment, offset.encodeLittleEndian) {
      override val operandByteSize: FarPointerSize = FarPointerSize.FarWord
    }
}
