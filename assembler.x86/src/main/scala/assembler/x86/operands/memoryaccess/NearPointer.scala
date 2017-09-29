package assembler.x86.operands.memoryaccess

import assembler.Address
import assembler.x86.operands.{FixedSizeOperand, Operand, OperandSize}

sealed class NearPointer(val offset: X86Offset) extends Operand with Address[X86Offset] with FixedSizeOperand {
  val operandByteSize: OperandSize = offset.size

  override def toString: String = offset.toString

  def encodeBytes: List[Byte] = offset.encodeByte

  override def add(offset: X86Offset): Nothing = ???
}

object ShortPointer {
  def apply(offset: Byte) = new NearPointer(ShortOffset(offset))
}

object LongPointer {
  def apply(offset: Short) = new NearPointer(RealModeLongOffset(offset))
  def apply(offset: Int) = new NearPointer(ProtectedModeLongOffset(offset))
}