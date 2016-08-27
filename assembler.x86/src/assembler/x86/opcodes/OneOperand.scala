package assembler.x86.opcodes

import assembler.x86.ParameterPosition
import assembler.x86.ProcessorMode
import assembler.x86.operands.memoryaccess.MemoryLocation
import assembler.x86.operands.FixedSizeParameter
import assembler.x86.operands.ImmediateValue
import assembler.x86.operands.Operand
import assembler.x86.instructions.FixedSizeX86Instruction
import assembler.x86.X86ContextOneOperand
import assembler.x86.operands.ModRMEncodableOperand
import assembler.x86.operands.ModRMEncodableOperand
import assembler.x86.operands.memoryaccess.MemoryLocation
import assembler.MemoryPage
import assembler.Label

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
      override def encode()(implicit page: MemoryPage): List[Byte] = {
        val operandSize = getOperandSize(operand)
        Opcode.optionalOperandSizePrefix(operandSize) :::
          Opcode.optionalAddressSizePrefix(getAddressSize(operand)) :::
          Opcode.optionalRexPrefix(operandSize, operand.getRexRequirements(parameterPosition), includeRexW) :::
          getCode(operand)
      }
      override def toString() = s"${OneOperand.this.mnemonic} ${operand.toString()}"
    }
  }

  def asTwoOperandOpcode[Operand2Type <: Operand](validateExtension: PartialFunction[(OperandType, Operand2Type, ProcessorMode), Boolean], includeRexW: Boolean = true): TwoOperand[OperandType, Operand2Type] =
    new TwoOperand[OperandType, Operand2Type](OneOperand.this.parameterPosition, ParameterPosition.None, mnemonic) {

      override def validate(operand1: OperandType, operand2: Operand2Type)(implicit processorMode: ProcessorMode): Boolean =
        super.validate(operand1, operand2) && validateExtension(operand1, operand2, processorMode)

      override def getCode(operand1: OperandType, operand2: Operand2Type) =
        OneOperand.this.getCode(operand1)
    }

  def withOffset(includeRexW: Boolean = true): TwoOperand[OperandType, MemoryLocation] =
    new TwoOperand[OperandType, MemoryLocation](OneOperand.this.parameterPosition, ParameterPosition.None, mnemonic) {

      override def getCode(operand: OperandType, memoryLocation: MemoryLocation): List[Byte] =
        OneOperand.this.getCode(operand) ::: memoryLocation.displacement
    }

  def withImmediate(validateExtension: PartialFunction[(OperandType, ImmediateValue, ProcessorMode), Boolean] = TwoOperand.valid, includeRexW: Boolean = true): TwoOperand[OperandType, ImmediateValue] =
    new TwoOperand[OperandType, ImmediateValue](OneOperand.this.parameterPosition, ParameterPosition.None, mnemonic) {

      override def validate(operand: OperandType, immediate: ImmediateValue)(implicit processorMode: ProcessorMode): Boolean =
        super.validate(operand, immediate) && validateExtension(operand, immediate, processorMode)

      override def getCode(operand: OperandType, immediate: ImmediateValue): List[Byte] =
        OneOperand.this.getCode(operand) ::: immediate.value
    }

}

object OneOperand {
  def valid[OperandType <: Operand]: PartialFunction[(OperandType, ProcessorMode), Boolean] = { case _ => true }

  def getModRM(rValue: Byte, operandRM: ModRMEncodableOperand): List[Byte] = operandRM match {
    case location: MemoryLocation => getModRMByte(rValue, operandRM) :: location.displacement
    case _ => getModRMByte(rValue, operandRM) :: Nil
  }

  def getModRMByte(rValue: Byte, operandRM: ModRMEncodableOperand) =
    (((operandRM.modValue & 3) << 6) | ((rValue & 7) << 3) | (operandRM.registerOrMemoryModeCode & 7)).toByte
}