package assembler.x86.instructions.arithmetic

import assembler.x86.ProcessorMode
import assembler.x86.operations.ModRMStaticOperation
import assembler.x86.operands.ValueSize
import assembler.x86.operands.FixedSizeOperand
import assembler.x86.operands.ModRMEncodableOperand

object Not {
  implicit val opcode = "not"

  private def RM8(operand: ModRMEncodableOperand with FixedSizeOperand)(implicit processorMode: ProcessorMode) =
    new ModRMStaticOperation(operand, 0xF6.toByte :: Nil, 2, opcode)
  private def RM16(operand: ModRMEncodableOperand with FixedSizeOperand)(implicit processorMode: ProcessorMode) =
    new ModRMStaticOperation(operand, 0xF7.toByte :: Nil, 2, opcode)

  def apply(operand: ModRMEncodableOperand with FixedSizeOperand)(implicit processorMode: ProcessorMode) = operand.operandByteSize match {
    case ValueSize.Byte => RM8(operand)
    case _ => RM16(operand)
  }
}