package assembler.x86.operations

import assembler.Label
import assembler.x86.operands._
import assembler.x86.operands.memoryaccess.{MemoryLocation => MemoryLocationType}
import assembler.x86.{ParameterPosition, ProcessorMode, RexRequirement}

class ModRMStatic(label: Label, val operandRM: ModRMEncodableOperand,
                  override val code: List[Byte],
                  val rValue: Byte,
                  override val mnemonic: String,
                  override val includeRexW: Boolean = true)(override implicit val processorMode: ProcessorMode)
  extends X86Operation(label) {

  override def operands: List[Operand] = operandRM :: Nil

  override def validate(): Unit = {
    super.validate()
    assume(operandRM.isValidForMode(processorMode))
  }

  override def operandSize: OperandSize = (super.operandSize, operandRM) match {
    case (OperandSize.Unknown, fixed: FixedSizeOperand) => fixed.operandByteSize
    case _ => super.operandSize
  }

  override def addressSize: OperandSize = (super.addressSize, operandRM) match {
    case (OperandSize.Unknown, address: MemoryLocationType) => address.addressSize
    case _ => super.operandSize
  }

  override def segmentOverride: Option[SegmentRegister] = operandRM match {
    case location: MemoryLocationType => location.segmentOverride
    case _ => None
  }

  override def rexRequirements: List[RexRequirement] = super.rexRequirements :::
    operandRM.getRexRequirements(ParameterPosition.OperandRM)

  override def encodeByte: Seq[Byte] =
    super.encodeByte ++ operandRM.getExtendedBytes(rValue)
}
