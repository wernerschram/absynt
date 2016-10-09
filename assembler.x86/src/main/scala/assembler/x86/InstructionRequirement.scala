package assembler.x86

import assembler.x86.opcodes.Opcode

class ParameterPosition(val rexRequirement: Option[RexExtendedRequirement])

object ParameterPosition {
  case object Base extends ParameterPosition(Some(RexExtendedRequirement.instanceBase))
  case object Index extends ParameterPosition(Some(RexExtendedRequirement.instanceIndex))
  case object OpcodeReg extends ParameterPosition(Some(RexExtendedRequirement.instanceOpcodeReg))
  case object OperandR extends ParameterPosition(Some(RexExtendedRequirement.instanceOperandR))
  case object OperandRM extends ParameterPosition(Some(RexExtendedRequirement.instanceOperandRM))
  case object NotEncoded extends ParameterPosition(None)
}

sealed protected class RexExtendedRequirement(val rexBitmask: Byte)

object RexExtendedRequirement {
  val instanceIndex = new RexExtendedRequirement(0x02)
  val instanceBase = new RexExtendedRequirement(0x01)
  val instanceOpcodeReg = new RexExtendedRequirement(0x01)
  val instanceOperandR = new RexExtendedRequirement(0x04)
  val instanceOperandRM = new RexExtendedRequirement(0x01)
}
