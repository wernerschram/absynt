package assembler.x86.operands.memoryaccess

import assembler.ListExtensions._
import assembler.x86.operands.FixedSizeOperand
import assembler.x86.operands.Operand

class NearPointer private(val displacement: List[Byte]) extends Operand with FixedSizeOperand {
  assume(List(1, 2, 4).contains(displacement.length))
  val operandByteSize: Int = displacement.length

  override def toString() = s"0x${displacement.bigEndianHexString()}"
}

object NearPointer {
  def apply(displacement: List[Byte]) = new NearPointer(displacement)
}