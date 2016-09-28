package assembler.x86

import assembler.x86.opcodes.Opcode

class ParameterPosition

object ParameterPosition {
  case object Base extends ParameterPosition
  case object Index extends ParameterPosition
  case object OpcodeReg extends ParameterPosition
  case object OperandR extends ParameterPosition
  case object OperandRM extends ParameterPosition
  case object None extends ParameterPosition
}

sealed protected class RexExtendedRequirement(val position: ParameterPosition, val rexBitmask: Byte)

object RexExtendedRequirement {
  def instance(position: ParameterPosition) = position match {
    case ParameterPosition.Base => instanceBase
    case ParameterPosition.Index => instanceIndex
    case ParameterPosition.OpcodeReg => instanceOpcodeReg
    case ParameterPosition.OperandR => instanceOperandR
    case ParameterPosition.OperandRM => instanceOperandRM
  }

  val instanceIndex = new RexExtendedRequirement(ParameterPosition.Index, 0x02)
  val instanceBase = new RexExtendedRequirement(ParameterPosition.Base, 0x01)
  val instanceOpcodeReg = new RexExtendedRequirement(ParameterPosition.OpcodeReg, 0x01)
  val instanceOperandR = new RexExtendedRequirement(ParameterPosition.OperandR, 0x04)
  val instanceOperandRM = new RexExtendedRequirement(ParameterPosition.OperandRM, 0x01)
}
