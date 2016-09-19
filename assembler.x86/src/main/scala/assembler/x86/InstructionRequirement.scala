package assembler.x86

sealed protected class RexExtendedRequirement(val position: ParameterPosition)

class ParameterPosition

object ParameterPosition {
  case object Base extends ParameterPosition
  case object Index extends ParameterPosition
  case object OpcodeReg extends ParameterPosition
  case object OperandR extends ParameterPosition
  case object OperandRM extends ParameterPosition
  case object None extends ParameterPosition
}

object RexExtendedRequirement {
  def instance(position: ParameterPosition) = position match {
    case ParameterPosition.Base => instanceBase
    case ParameterPosition.Index => instanceIndex
    case ParameterPosition.OpcodeReg => instanceOpcodeReg
    case ParameterPosition.OperandR => instanceOperandR
    case ParameterPosition.OperandRM => instanceOperandRM
  }

  val instanceIndex = new RexExtendedRequirement(ParameterPosition.Index)
  val instanceBase = new RexExtendedRequirement(ParameterPosition.Base)
  val instanceOpcodeReg = new RexExtendedRequirement(ParameterPosition.OpcodeReg)
  val instanceOperandR = new RexExtendedRequirement(ParameterPosition.OperandR)
  val instanceOperandRM = new RexExtendedRequirement(ParameterPosition.OperandRM)
}