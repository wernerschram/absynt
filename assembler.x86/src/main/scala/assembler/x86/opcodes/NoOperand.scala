package assembler.x86.opcodes

import assembler.memory.MemoryPage
import assembler.x86.ParameterPosition
import assembler.x86.ProcessorMode
import assembler.x86.instructions.FixedSizeX86Operation
import assembler.x86.operands.ImmediateValue
import assembler.x86.operands.ModRMEncodableOperand
import assembler.x86.operands.Operand
import assembler.x86.operands.memoryaccess.FarPointer
import assembler.x86.operands.memoryaccess.NearPointer
import assembler.x86.operands.Register

abstract trait NoOperand {

  val mnemonic: String

  def validate()(implicit processorMode: ProcessorMode): Boolean = true

  def getCode(): List[Byte]

  def apply()(implicit processorMode: ProcessorMode): FixedSizeX86Operation = {
    new FixedSizeX86Operation() {
      assume(validate())
      override def encodeByte()(implicit page: MemoryPage): List[Byte] = getCode()
      override def toString() = NoOperand.this.toString()
    }
  }

  override def toString() = mnemonic

  def asOneOperandOpcode[OperandType <: Operand](validateExtension: PartialFunction[(OperandType, ProcessorMode), Boolean]): OneOperand[OperandType] =
    new OneOperand[OperandType] {
      val parameterPosition = ParameterPosition.NotEncoded
      val mnemonic = NoOperand.this.mnemonic

      override def validate(operand: OperandType)(implicit processorMode: ProcessorMode): Boolean =
        super.validate(operand) && validateExtension(operand, processorMode)

      override def getCode(operand: OperandType): List[Byte] =
        NoOperand.this.getCode()
    }

  def asTwoOperandOpcode[Operand1Type <: Operand, Operand2Type <: Operand](validateExtension: PartialFunction[(Operand1Type, Operand2Type, ProcessorMode), Boolean]): TwoOperand[Operand1Type, Operand2Type] =
    new TwoOperand[Operand1Type, Operand2Type] {

      val parameter1Position = ParameterPosition.NotEncoded
      val parameter2Position = ParameterPosition.NotEncoded
      val mnemonic = NoOperand.this.mnemonic

      override def validate(operand1: Operand1Type, operand2: Operand2Type)(implicit processorMode: ProcessorMode): Boolean =
        super.validate(operand1, operand2) && validateExtension(operand1, operand2, processorMode)

      def getCode(operand1: Operand1Type, operand2: Operand2Type): List[Byte] =
        NoOperand.this.getCode()
    }

  def withImmediate(validateExtension: PartialFunction[(ImmediateValue, ProcessorMode), Boolean] = OneOperand.valid): OneOperand[ImmediateValue] =
    new OneOperand[ImmediateValue] {

      val parameterPosition = ParameterPosition.NotEncoded
      val mnemonic = NoOperand.this.mnemonic

      override def validate(immediate: ImmediateValue)(implicit processorMode: ProcessorMode): Boolean =
        super.validate(immediate) && validateExtension(immediate, processorMode)

      override def getCode(immediate: ImmediateValue): List[Byte] =
        NoOperand.this.getCode ::: immediate.value
    }

  def withNearPointer(validateExtension: PartialFunction[(NearPointer, ProcessorMode), Boolean] = OneOperand.valid): OneOperand[NearPointer] =
    new OneOperand[NearPointer] {

      val parameterPosition = ParameterPosition.NotEncoded
      val mnemonic = NoOperand.this.mnemonic

      override def validate(pointer: NearPointer)(implicit processorMode: ProcessorMode): Boolean =
        super.validate(pointer) && validateExtension(pointer, processorMode)

      override def getCode(pointer: NearPointer): List[Byte] =
        NoOperand.this.getCode ::: pointer.displacement
    }

  def withFarPointer(): OneOperand[FarPointer] =
    new OneOperand[FarPointer] {

      val parameterPosition = ParameterPosition.NotEncoded
      val mnemonic = NoOperand.this.mnemonic

      override def getCode(pointer: FarPointer): List[Byte] =
        NoOperand.this.getCode ::: pointer.offset ::: pointer.segment
    }

  def withModRM(rValue: Byte) =
    new OneOperand[ModRMEncodableOperand] {

      val parameterPosition = ParameterPosition.OperandRM
      val mnemonic = NoOperand.this.mnemonic

      override def getCode(operandRM: ModRMEncodableOperand): List[Byte] =
        NoOperand.this.getCode ::: operandRM.getExtendedBytes(rValue)
    }

  def withImplicitRegisters(register1: Register, register2: Register) =
    new NoOperand {

      val mnemonic = NoOperand.this.mnemonic

      override def getCode(): List[Byte] = NoOperand.this.getCode
      override def toString() = s"${NoOperand.this.toString()} ${register1.toString()}, ${register2.toString()}"
    }
}