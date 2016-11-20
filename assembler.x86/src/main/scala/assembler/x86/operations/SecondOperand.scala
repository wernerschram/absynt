package assembler.x86.operations

import assembler.x86.operands.FixedSizeParameter
import assembler.x86.operands.memoryaccess.MemoryLocation
import assembler.memory.MemoryPage
import assembler.x86.ParameterPosition
import assembler.x86.ProcessorMode
import assembler.x86.operands.Operand
import assembler.x86.instructions.FixedSizeX86Operation
import assembler.x86.operands.SegmentRegister
import assembler.x86.instructions.FixedSizeX86Operation
import assembler.x86.instructions.FixedSizeX86Operation2

trait SecondOperand[Operand1Type <: Operand, Operand2Type <: Operand] extends FixedSizeX86Operation2 {

  self: OneOperandOperation[Operand1Type] =>

  val code: List[Byte]

  val operand2: Operand2Type

  abstract override def validate = {
    super.validate
    assume(operand2.isValidForMode(processorMode))
  }

  implicit val processorMode: ProcessorMode

  val parameter2Position: ParameterPosition
  val mnemonic: String

  override lazy val operandSize: Option[Int] = (operand1, operand2) match {
    case (fixed: FixedSizeParameter, _) => Some(fixed.operandByteSize)
    case (_, fixed: FixedSizeParameter) => Some(fixed.operandByteSize)
    case _ => None
  }

  override lazy val addressSize: Option[Int] = (operand1, operand2) match {
    case (address: MemoryLocation, _) => Some(address.addressSize)
    case (_, address: MemoryLocation) => Some(address.addressSize)
    case _ => None
  }

  override lazy val segmentOverride: Option[SegmentRegister] = (operand1, operand2) match {
    case (location: MemoryLocation, _) => location.getSegmentOverride
    case (_, location: MemoryLocation) => location.getSegmentOverride
    case _ => None
  }

  override lazy val rexRequirements = operand1.getRexRequirements(parameter1Position) ::: operand2.getRexRequirements(parameter2Position)

}
