package assembler.x86

class ParameterPosition(val rexRequirement: Option[RexRequirement])

object ParameterPosition {

  case object Base extends ParameterPosition(Some(RexRequirement.instanceBase))
  case object Index extends ParameterPosition(Some(RexRequirement.instanceIndex))
  case object OpcodeReg extends ParameterPosition(Some(RexRequirement.instanceOpcodeReg))
  case object OperandR extends ParameterPosition(Some(RexRequirement.instanceOperandR))
  case object OperandRM extends ParameterPosition(Some(RexRequirement.instanceOperandRM))
  case object NotEncoded extends ParameterPosition(None)

}
