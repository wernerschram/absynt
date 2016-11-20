package assembler.x86.operations

import assembler.x86.ParameterPosition
import assembler.x86.operands.EncodableOperand
import assembler.x86.operands.ImmediateValue
import assembler.x86.ProcessorMode
import assembler.memory.MemoryPage
import assembler.x86.operands.EncodableRegister
import assembler.x86.operands.ModRMEncodableOperand
import assembler.x86.operands.Operand
import assembler.x86.operands.FixedSizeParameter
import assembler.x86.operands.memoryaccess.MemoryLocation
import assembler.x86.operands.SegmentRegister
import assembler.x86.instructions.FixedSizeX86Operation2

class ModRRMStaticOperation[RegisterType <: EncodableRegister](
  val operand1: RegisterType,
  val operand2: ModRMEncodableOperand,
  override val code: List[Byte],
  override val mnemonic: String,
  override val includeRexW: Boolean = true)(override implicit val processorMode: ProcessorMode)
    extends FixedSizeX86Operation2 {

  override def operands: List[Operand] = operand1 :: operand2 :: Nil

  override def validate = {
    assume(operand1.isValidForMode(processorMode))
    assume(operand2.isValidForMode(processorMode))
  }

  override def operandSize: Option[Int] = (operand1, operand2) match {
    case (fixed: FixedSizeParameter, _) => Some(fixed.operandByteSize)
    case (_, fixed: FixedSizeParameter) => Some(fixed.operandByteSize)
    case _ => None
  }

  override def addressSize: Option[Int] = operand2 match {
    case address: MemoryLocation => Some(address.addressSize)
    case _ => None
  }

  override def segmentOverride: Option[SegmentRegister] = operand2 match {
    case location: MemoryLocation => location.getSegmentOverride
    case _ => None
  }

  override def rexRequirements = operand1.getRexRequirements(ParameterPosition.OperandR) ::: operand2.getRexRequirements(ParameterPosition.OperandRM)

  override def encodeByte()(implicit page: MemoryPage): List[Byte] = {
    super.encodeByte() ::: operand2.getExtendedBytes(operand1)
  }
}
