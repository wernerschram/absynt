package assembler.x86.operands

import assembler.x86.RexExtendedRequirement
import assembler.x86.ParameterPosition

trait ModRMEncodableOperand extends Operand {
  val modValue: Byte
  val registerOrMemoryModeCode: Byte

  private def getModRM(rValue: Byte): Byte = (((modValue & 3) << 6) | ((rValue & 7) << 3) | (registerOrMemoryModeCode & 7)).toByte

  def getExtendedBytes(rValue: Byte): List[Byte] = getModRM(rValue)  :: Nil
  def getRexRequirements(position: ParameterPosition): List[RexExtendedRequirement] = Nil // linter:ignore UnusedParameter
}

trait FixedSizeOperand {
  val operandByteSize: OperandSize
}

trait FixedSizeModRMEncodableOperand extends ModRMEncodableOperand with FixedSizeOperand
