package assembler.x86.operands

import assembler.x86.{ParameterPosition, RexRequirement}

trait ModRMEncodableOperand extends Operand {
  val modValue: Byte
  val registerOrMemoryModeCode: Byte

  def getExtendedBytes(rValue: Byte): Seq[Byte] = Seq(getModRM(rValue))

  private def getModRM(rValue: Byte): Byte = (((modValue & 3) << 6) | ((rValue & 7) << 3) | (registerOrMemoryModeCode & 7)).toByte

  def getRexRequirements(position: ParameterPosition): List[RexRequirement] = Nil // linter:ignore UnusedParameter
}

trait FixedSizeOperand {
  val operandByteSize: OperandSize
}
