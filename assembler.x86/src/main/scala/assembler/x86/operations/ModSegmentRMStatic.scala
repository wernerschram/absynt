package assembler.x86.operations

import assembler.memory.MemoryPage
import assembler.x86.ParameterPosition
import assembler.x86.ProcessorMode
import assembler.x86.operands.ModRMEncodableOperand
import assembler.x86.operands.SegmentRegister
import assembler.x86.operands.FixedSizeEncodableOperand
import assembler.x86.operands.Operand
import assembler.x86.operands.FixedSizeParameter
import assembler.x86.operands.memoryaccess.MemoryLocation

class ModSegmentRMStaticOperation(
  override val operand1: SegmentRegister,
  val operand2: ModRMEncodableOperand,
  override val code: List[Byte],
  override val mnemonic: String,
  override val includeRexW: Boolean = true)(override implicit val processorMode: ProcessorMode)
    extends OneOperandOperation[SegmentRegister] {

  override val parameter1Position = ParameterPosition.OperandR

  override def operands: List[Operand] = operand1 :: operand2 :: Nil

  override def validate = {
    super.validate
    assume(operand2.isValidForMode(processorMode))
  }

  override def operandSize: Option[Int] = (super.operandSize, operand2) match {
    case (size: Some[Int], _) => size
    case (None, fixed: FixedSizeParameter) => Some(fixed.operandByteSize)
    case _ => None
  }

  override def addressSize: Option[Int] = (super.addressSize, operand2) match {
    case (size: Some[Int], _) => size
    case (None, address: MemoryLocation) => Some(address.addressSize)
    case _ => None
  }

  override def segmentOverride: Option[SegmentRegister] = (super.segmentOverride, operand2) match {
    case (segment: Some[SegmentRegister], _) => segment
    case (None, location: MemoryLocation) => location.getSegmentOverride
    case _ => None
  }

  override def rexRequirements = super.rexRequirements ::: operand2.getRexRequirements(ParameterPosition.OperandRM)

  override def encodeByte()(implicit page: MemoryPage): List[Byte] = {
    super.encodeByte() ::: operand2.getExtendedBytes(operand1)
  }
}
