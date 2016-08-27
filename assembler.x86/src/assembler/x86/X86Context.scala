package assembler.x86

import assembler.x86.operands.Operand

abstract class Context 

abstract class X86Context extends Context {
  implicit val processorMode: ProcessorMode
}

abstract class X86ContextOneOperand[OperandType <: Operand](
    val operandValue: OperandType)
  extends X86Context 


abstract class X86ContextTwoOperand[Operand1Type <: Operand, Operand2Type <: Operand]( 
    val operand1Value: Operand1Type,
    val operand2Value: Operand2Type) 
  extends X86Context