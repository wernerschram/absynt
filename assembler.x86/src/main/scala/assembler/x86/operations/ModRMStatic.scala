package assembler.x86.operations

import assembler.memory.MemoryPage
import assembler.x86.ParameterPosition
import assembler.x86.ProcessorMode
import assembler.x86.instructions.FixedSizeX86Operation2
import assembler.x86.operands.FixedSizeOperand
import assembler.x86.operands.ModRMEncodableOperand
import assembler.x86.operands.Operand
import assembler.x86.operands.SegmentRegister
import assembler.x86.operands.memoryaccess.{MemoryLocation => MemoryLocationType}

class ModRMStaticOperation(
  val operandRM: ModRMEncodableOperand,
  override val code: List[Byte],
  val rValue: Byte,
  override val mnemonic: String,
  override val includeRexW: Boolean = true)(override implicit val processorMode: ProcessorMode)
    extends FixedSizeX86Operation2 {

  override def operands: List[Operand] = operandRM :: Nil

  override def validate = {
    super.validate
    assume(operandRM.isValidForMode(processorMode))
  }

  override def operandSize: Option[Int] = operandRM match {
    case fixed: FixedSizeOperand => Some(fixed.operandByteSize)
    case _ => None
  }

  override def addressSize: Option[Int] = operandRM match {
    case address: MemoryLocationType => Some(address.addressSize)
    case _ => None
  }

  override def segmentOverride: Option[SegmentRegister] = operandRM match {
    case location: MemoryLocationType => location.getSegmentOverride
    case _ => None
  }

  override def rexRequirements = operandRM.getRexRequirements(ParameterPosition.OperandRM)

  override def encodeByte()(implicit page: MemoryPage): List[Byte] = {
    super.encodeByte() ::: operandRM.getExtendedBytes(rValue)
  }
}
