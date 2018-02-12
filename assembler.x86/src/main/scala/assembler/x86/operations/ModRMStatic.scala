package assembler.x86.operations

import assembler.x86.operands._
import assembler.x86.operands.memoryaccess.{MemoryLocation => MemoryLocationType}
import assembler.x86.operations.OperandInfo.OperandOrder.OperandOrder
import assembler.x86.{ParameterPosition, ProcessorMode, RexRequirement}

abstract class ModRMStatic(val operandRM: ModRMEncodableOperand,
                  override val code: Seq[Byte],
                  val rValue: Byte,
                  override val mnemonic: String,
                  override val includeRexW: Boolean = true)(override implicit val processorMode: ProcessorMode)
  extends X86Operation {

  def operandRMOrder: OperandOrder

  override def operands: Seq[OperandInfo] = Seq(OperandInfo.rmRegisterOrMemory(operandRM, operandRMOrder))

  override def validate(): Unit = {
    super.validate()
    assume(operandRM.isValidForMode(processorMode))
  }

  override def segmentOverride: Option[SegmentRegister] = operandRM match {
    case location: MemoryLocationType => location.segmentOverride
    case _ => None
  }

  override def rexRequirements: Seq[RexRequirement] = super.rexRequirements ++
    operandRM.getRexRequirements(ParameterPosition.OperandRM)

  override def encodeByte: Seq[Byte] =
    super.encodeByte ++ operandRM.getExtendedBytes(rValue)
}

