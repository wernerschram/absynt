package assembler.x86.instructions.arithmetic

import assembler.x86.ProcessorMode
import assembler.x86.operands.FixedSizeModRMEncodableOperand
import assembler.x86.operations.ModRMStaticOperation
import assembler.x86.operands.ValueSize

object Not {
  implicit val opcode = "not"

  private def RM8(operand: FixedSizeModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRMStaticOperation(operand, 0xF6.toByte :: Nil, 2, opcode)
  private def RM16(operand: FixedSizeModRMEncodableOperand)(implicit processorMode: ProcessorMode) =
    new ModRMStaticOperation(operand, 0xF7.toByte :: Nil, 2, opcode)

  def apply(operand: FixedSizeModRMEncodableOperand)(implicit processorMode: ProcessorMode) = operand.operandByteSize match {
    case ValueSize.Byte => RM8(operand)
    case _ => RM16(operand)
  }
}