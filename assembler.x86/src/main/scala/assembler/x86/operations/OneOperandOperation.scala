package assembler.x86.operations

import assembler.x86.operands.FixedSizeParameter
import assembler.x86.operands.memoryaccess.MemoryLocation
import assembler.memory.MemoryPage
import assembler.x86.ParameterPosition
import assembler.x86.ProcessorMode
import assembler.x86.operands.Operand
import assembler.x86.instructions.FixedSizeX86Operation
import assembler.x86.operands.SegmentRegister
import assembler.x86.RexExtendedRequirement
import assembler.x86.instructions.FixedSizeX86Operation2

trait OneOperandOperation[OperandType <: Operand] extends FixedSizeX86Operation2 {
  assume(validate(operand1))

  def validate(operand: OperandType)(implicit processorMode: ProcessorMode): Boolean =
    operand.isValidForMode(processorMode)

  val code: List[Byte]

  val operand1: OperandType
  implicit val processorMode: ProcessorMode

  val parameter1Position: ParameterPosition
  val mnemonic: String

  override lazy val operandSize: Option[Int] = operand1 match {
    case fixed: FixedSizeParameter => Some(fixed.operandByteSize)
    case _ => None
  }

  override lazy val addressSize: Option[Int] = operand1 match {
    case address: MemoryLocation => Some(address.addressSize)
    case _ => None
  }

  override lazy val segmentOverride: Option[SegmentRegister] = operand1 match {
    case location: MemoryLocation => location.getSegmentOverride
    case _ => None
  }

  override lazy val rexRequirements = operand1.getRexRequirements(parameter1Position)

  override def toString() = s"${mnemonic} ${operand1.toString()}"

}
