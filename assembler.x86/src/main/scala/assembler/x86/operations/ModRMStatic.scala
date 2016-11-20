package assembler.x86.operations

import assembler.x86.ParameterPosition
import assembler.x86.operands.EncodableOperand
import assembler.x86.operands.ImmediateValue
import assembler.x86.ProcessorMode
import assembler.memory.MemoryPage
import assembler.x86.operands.Operand
import assembler.x86.instructions.FixedSizeX86Operation2
import assembler.x86.operands.FixedSizeParameter
import assembler.x86.operands.memoryaccess.MemoryLocation
import assembler.x86.operands.SegmentRegister

class ModRMStaticOperation(
  val operand1: EncodableOperand,
  override val code: List[Byte],
  val rValue: Byte,
  override val mnemonic: String,
  override val includeRexW: Boolean = true)(override implicit val processorMode: ProcessorMode)
    extends FixedSizeX86Operation2 {

  override def operands: List[Operand] = operand1 :: Nil

  def validate = {
    assume(operand1.isValidForMode(processorMode))
  }

  override def operandSize: Option[Int] = operand1 match {
    case fixed: FixedSizeParameter => Some(fixed.operandByteSize)
    case _ => None
  }

  override def addressSize: Option[Int] = operand1 match {
    case address: MemoryLocation => Some(address.addressSize)
    case _ => None
  }

  override def segmentOverride: Option[SegmentRegister] = operand1 match {
    case location: MemoryLocation => location.getSegmentOverride
    case _ => None
  }

  override def rexRequirements = operand1.getRexRequirements(ParameterPosition.OperandRM)

  override def encodeByte()(implicit page: MemoryPage): List[Byte] = {
    super.encodeByte() ::: operand1.getExtendedBytes(rValue)
  }
}
