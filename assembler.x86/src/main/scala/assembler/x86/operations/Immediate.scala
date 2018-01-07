package assembler.x86.operations

import assembler.x86.operands.{ImmediateValue, Operand, OperandSize}

trait Immediate extends X86Operation {

  self: X86Operation =>
  def immediate: ImmediateValue

  abstract override def operands: List[Operand] = super.operands ::: immediate :: Nil

  abstract override def operandSize: OperandSize = super.operandSize match {
    case OperandSize.Unknown => immediate.operandByteSize
    case _ => super.operandSize
  }

  abstract override def encodeByte: List[Byte] = {
    super.encodeByte ++ immediate.value
  }

}
