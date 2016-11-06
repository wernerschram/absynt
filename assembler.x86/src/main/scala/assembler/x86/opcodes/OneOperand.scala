package assembler.x86.opcodes

import assembler.memory.MemoryPage
import assembler.x86.ParameterPosition
import assembler.x86.ProcessorMode
import assembler.x86.instructions.FixedSizeX86Instruction
import assembler.x86.operands.FixedSizeParameter
import assembler.x86.operands.ImmediateValue
import assembler.x86.operands.ModRMEncodableOperand
import assembler.x86.operands.Operand
import assembler.x86.operands.memoryaccess.MemoryLocation

abstract class OneOperand[OperandType <: Operand](val parameterPosition: ParameterPosition, val mnemonic: String) {
  val includeRexW: Boolean = true

  def validate(operand: OperandType)(implicit processorMode: ProcessorMode): Boolean =
    operand.isValidForMode(processorMode)

  def getCode(operand: OperandType): List[Byte]

  def getOperandSize(operand: Operand): Option[Int] = operand match {
    case fixed: FixedSizeParameter => Some(fixed.operandByteSize)
    case _ => None
  }

  def getAddressSize(operand: Operand): Option[Int] = operand match {
    case address: MemoryLocation => Some(address.addressSize)
    case _ => return None
  }

  def apply(operand: OperandType)(implicit processorMode: ProcessorMode): FixedSizeX86Instruction = {
    new FixedSizeX86Instruction() {
      assume(validate(operand))
      override def encodeByte()(implicit page: MemoryPage): List[Byte] = {
        val operandSize = getOperandSize(operand)
        Opcode.optionalOperandSizePrefix(operandSize) :::
          Opcode.optionalAddressSizePrefix(getAddressSize(operand)) :::
          Opcode.optionalRexPrefix(operandSize, operand.getRexRequirements(parameterPosition), includeRexW) :::
          getCode(operand)
      }
      override def toString() = s"${OneOperand.this.mnemonic} ${operand.toString()}"
    }
  }

  def withOffset(reversed: Boolean = false): TwoOperand[OperandType, MemoryLocation] =
    if (!reversed)
      new TwoOperand[OperandType, MemoryLocation](OneOperand.this.parameterPosition, ParameterPosition.NotEncoded, mnemonic) {

        override def getCode(operand: OperandType, memoryLocation: MemoryLocation): List[Byte] =
          OneOperand.this.getCode(operand) ::: memoryLocation.displacement
      }
    else
      new TwoOperand[OperandType, MemoryLocation](OneOperand.this.parameterPosition, ParameterPosition.NotEncoded, mnemonic) with reversedOperands[OperandType, MemoryLocation] {

        override def getCode(operand: OperandType, memoryLocation: MemoryLocation): List[Byte] =
          OneOperand.this.getCode(operand) ::: memoryLocation.displacement
      }

  def withImmediate(validateExtension: PartialFunction[(OperandType, ImmediateValue, ProcessorMode), Boolean] = TwoOperand.valid): TwoOperand[OperandType, ImmediateValue] =
    new TwoOperand[OperandType, ImmediateValue](OneOperand.this.parameterPosition, ParameterPosition.NotEncoded, mnemonic) with reversedOperands[OperandType, ImmediateValue] {

      override def validate(operand: OperandType, immediate: ImmediateValue)(implicit processorMode: ProcessorMode): Boolean =
        super.validate(operand, immediate) && validateExtension(operand, immediate, processorMode)

      override def getCode(operand: OperandType, immediate: ImmediateValue): List[Byte] =
        OneOperand.this.getCode(operand) ::: immediate.value
    }

}

object OneOperand {
  def valid[OperandType <: Operand]: PartialFunction[(OperandType, ProcessorMode), Boolean] = { case _ => true }
}