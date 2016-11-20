package assembler.x86.operations

import assembler.x86.operands.Operand
import assembler.x86.instructions.FixedSizeX86Operation2

//trait ReversedOperands[Operand1Type <: Operand, Operand2Type <: Operand]
//    extends OneOperandOperation[Operand1Type]
//    with SecondOperand[Operand1Type, Operand2Type] {
//  override def toString =
//    s"${mnemonic} ${operand1}, ${operand2}"
//}

trait ReversedOperands
    extends FixedSizeX86Operation2 {
  override def toString() = s"${mnemonic} ${operands.map { operand => operand.toString() }.mkString(", ")}"
}
