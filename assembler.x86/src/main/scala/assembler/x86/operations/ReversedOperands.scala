package assembler.x86.operations

import assembler.x86.operands.Operand

trait ReversedOperands[Operand1Type <: Operand, Operand2Type <: Operand]
    extends OneOperandOperation[Operand1Type]
    with SecondOperand[Operand1Type, Operand2Type] {
  override def toString =
    s"${mnemonic} ${operand1}, ${operand2}"
}
