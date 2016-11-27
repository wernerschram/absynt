package assembler.x86.operations

import assembler.memory.MemoryPage
import assembler.x86.operands.ImmediateValue
import assembler.x86.operands.OperandSize

trait Immediate extends X86Operation {

  self: X86Operation =>
  def immediate: ImmediateValue

  abstract override def operands = super.operands ::: immediate :: Nil

  abstract override def operandSize = super.operandSize match {
    case OperandSize.Unknown => immediate.operandByteSize
    case default => super.operandSize
  }

  abstract override def encodeByte()(implicit page: MemoryPage): List[Byte] = {
    super.encodeByte() ::: immediate.value
  }

}
