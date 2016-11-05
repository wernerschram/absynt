package assembler.x86.opcodes

import assembler.memory.MemoryPage
import assembler.x86.ParameterPosition
import assembler.x86.ProcessorMode
import assembler.x86.instructions.FixedSizeX86Instruction
import assembler.x86.operands.ImmediateValue
import assembler.x86.operands.ModRMEncodableOperand
import assembler.x86.operands.Operand
import assembler.x86.operands.memoryaccess.FarPointer
import assembler.x86.operands.memoryaccess.NearPointer
import assembler.x86.operands.registers.Register

abstract class NoOperand(val mnemonic: String) {
  def validate()(implicit processorMode: ProcessorMode): Boolean = true

  def getCode(): List[Byte]

  def apply()(implicit processorMode: ProcessorMode): FixedSizeX86Instruction = {
    new FixedSizeX86Instruction() {
      assume(validate())
      override def encodeByte()(implicit page: MemoryPage): List[Byte] = getCode()
      override def toString() = NoOperand.this.toString()
    }
  }

  override def toString() = mnemonic

  def asOneOperandOpcode[OperandType <: Operand](validateExtension: PartialFunction[(OperandType, ProcessorMode), Boolean]): OneOperand[OperandType] =
    new OneOperand[OperandType](ParameterPosition.NotEncoded, mnemonic) {

      override def validate(operand: OperandType)(implicit processorMode: ProcessorMode): Boolean =
        super.validate(operand) && validateExtension(operand, processorMode)

      override def getCode(operand: OperandType): List[Byte] =
        NoOperand.this.getCode()
    }

  def asTwoOperandOpcode[Operand1Type <: Operand, Operand2Type <: Operand](validateExtension: PartialFunction[(Operand1Type, Operand2Type, ProcessorMode), Boolean]): TwoOperand[Operand1Type, Operand2Type] =
    new TwoOperand[Operand1Type, Operand2Type](ParameterPosition.NotEncoded, ParameterPosition.NotEncoded, mnemonic) {

      override def validate(operand1: Operand1Type, operand2: Operand2Type)(implicit processorMode: ProcessorMode): Boolean =
        super.validate(operand1, operand2) && validateExtension(operand1, operand2, processorMode)

      def getCode(operand1: Operand1Type, operand2: Operand2Type): List[Byte] =
        NoOperand.this.getCode()
    }

  def withImmediate(validateExtension: PartialFunction[(ImmediateValue, ProcessorMode), Boolean] = OneOperand.valid): OneOperand[ImmediateValue] =
    new OneOperand[ImmediateValue](ParameterPosition.NotEncoded, mnemonic) {

      override def validate(immediate: ImmediateValue)(implicit processorMode: ProcessorMode): Boolean =
        super.validate(immediate) && validateExtension(immediate, processorMode)

      override def getCode(immediate: ImmediateValue): List[Byte] =
        NoOperand.this.getCode ::: immediate.value
    }

  def withNearPointer(validateExtension: PartialFunction[(NearPointer, ProcessorMode), Boolean] = OneOperand.valid): OneOperand[NearPointer] =
    new OneOperand[NearPointer](ParameterPosition.NotEncoded, mnemonic) {

      override def validate(pointer: NearPointer)(implicit processorMode: ProcessorMode): Boolean =
        super.validate(pointer) && validateExtension(pointer, processorMode)

      override def getCode(pointer: NearPointer): List[Byte] =
        NoOperand.this.getCode ::: pointer.displacement
    }

  def withFarPointer(): OneOperand[FarPointer] =
    new OneOperand[FarPointer](ParameterPosition.NotEncoded, mnemonic) {

      override def getCode(pointer: FarPointer): List[Byte] =
        NoOperand.this.getCode ::: pointer.offset ::: pointer.segment
    }

  def withModRM(rValue: Byte) =
    new OneOperand[ModRMEncodableOperand](ParameterPosition.OperandRM, mnemonic) {
      override def getCode(operandRM: ModRMEncodableOperand): List[Byte] =
        NoOperand.this.getCode ::: operandRM.getExtendedBytes(rValue)
    }

  def withImplicitRegisters(register1: Register, register2: Register) =
    new NoOperand(mnemonic) {
      override def getCode(): List[Byte] = NoOperand.this.getCode
      override def toString() = s"${NoOperand.this.toString()} ${register1.toString()}, ${register2.toString()}"
  }
}