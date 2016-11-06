package assembler.x86.opcodes

import assembler.memory.MemoryPage
import assembler.x86.ParameterPosition
import assembler.x86.ProcessorMode
import assembler.x86.instructions.FixedSizeX86Instruction
import assembler.x86.operands.FixedSizeParameter
import assembler.x86.operands.Operand
import assembler.x86.operands.memoryaccess.MemoryLocation
import assembler.x86.operands.SegmentRegister

abstract class TwoOperand[Operand1Type <: Operand, Operand2Type <: Operand](
    val parameter1Position: ParameterPosition,
    val parameter2Position: ParameterPosition,
    val mnemonic: String,
    reverse: Boolean = false) {
  val includeRexW: Boolean = true

  def validate(operand1: Operand1Type, operand2: Operand2Type)(implicit processorMode: ProcessorMode): Boolean =
    operand1.isValidForMode(processorMode) &&
      operand2.isValidForMode(processorMode)

  def getCode(operand1: Operand1Type, operand2: Operand2Type): List[Byte]

  def apply(operand1: Operand1Type, operand2: Operand2Type)(implicit processorMode: ProcessorMode): FixedSizeX86Instruction = {
    new FixedSizeX86Instruction() {
      assume(validate(operand1, operand2))
      override def encodeByte()(implicit page: MemoryPage): List[Byte] = {
        val rexRequirements = operand1.getRexRequirements(parameter1Position) ::: operand2.getRexRequirements(parameter2Position)

        Opcode.optionalSegmentOverridePrefix(TwoOperand.getSegmentOverride(operand1, operand2)) :::
          Opcode.optionalAddressSizePrefix(TwoOperand.getAddressSize(operand1, operand2)) :::
          Opcode.optionalOperandSizePrefix(TwoOperand.getOperandSize(operand1, operand2)) :::
          Opcode.optionalRexPrefix(TwoOperand.getOperandSize(operand1, operand2), rexRequirements, includeRexW) :::
          getCode(operand1, operand2)
      }

      override def toString() = TwoOperand.this.toString(operand1, operand2)
    }
  }

  def toString(operand1: Operand1Type, operand2: Operand2Type) =
    s"${mnemonic} ${operand2}, ${operand1}"

  def repeated() = new TwoOperand[Operand1Type, Operand2Type](parameter1Position, parameter2Position, s"REP ${mnemonic}") {
    override def getCode(operand1: Operand1Type, operand2: Operand2Type): List[Byte] =
      Opcode.RepeatPrefix :: TwoOperand.this.getCode(operand1, operand2)
  }
}

trait reversedOperands[Operand1Type <: Operand, Operand2Type <: Operand] extends TwoOperand[Operand1Type, Operand2Type] {
  override def toString(operand1: Operand1Type, operand2: Operand2Type) =
    s"${mnemonic} ${operand1}, ${operand2}"
}

object TwoOperand {
  def valid[OperandType, Operand2Type]: PartialFunction[(OperandType, Operand2Type, ProcessorMode), Boolean] = { case _ => true }

  def getOperandSize(operand1: Operand, operand2: Operand): Option[Int] = (operand1, operand2) match {
    case (fixed: FixedSizeParameter, _) => Some(fixed.operandByteSize)
    case (_, fixed: FixedSizeParameter) => Some(fixed.operandByteSize)
    case _ => return None
  }

  def getAddressSize(operand1: Operand, operand2: Operand): Option[Int] = (operand1, operand2) match {
    case (address: MemoryLocation, _) => Some(address.addressSize)
    case (_, address: MemoryLocation) => Some(address.addressSize)
    case _ => return None
  }

  def getSegmentOverride(operand1: Operand, operand2: Operand): Option[SegmentRegister] = (operand1, operand2) match {
    case (location: MemoryLocation, _) => location.getSegmentOverride
    case (_, location: MemoryLocation) => location.getSegmentOverride
    case _ => None
  }
}