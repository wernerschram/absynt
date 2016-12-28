package assembler.x86.operations

import assembler.memory.MemoryPage
import assembler.x86.ParameterPosition
import assembler.x86.ProcessorMode
import assembler.x86.operands.FixedSizeOperand
import assembler.x86.operands.ModRMEncodableOperand
import assembler.x86.operands.Operand
import assembler.x86.operands.SegmentRegister
import assembler.x86.operands.memoryaccess.{ MemoryLocation => MemoryLocationType }
import assembler.x86.operands.OperandSize

class ModRMStaticOperation(
  val operandRM: ModRMEncodableOperand,
  override val code: List[Byte],
  val rValue: Byte,
  override val mnemonic: String,
  override val includeRexW: Boolean = true)(override implicit val processorMode: ProcessorMode)
    extends X86Operation {

  override def operands: List[Operand] = operandRM :: Nil

  override def validate = {
    super.validate
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

  override def rexRequirements = super.rexRequirements :::
    operandRM.getRexRequirements(ParameterPosition.OperandRM)

  override def encodeByte()(implicit page: MemoryPage): List[Byte] = {
    super.encodeByte() ::: operandRM.getExtendedBytes(rValue)
  }
}
