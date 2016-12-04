package assembler.arm.operations

import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition.Condition
import assembler.arm.operands.Shifter
import assembler.arm.operands.registers.GeneralRegister
import assembler.memory.MemoryPage

class DataProcessingOperation(mnemonic: String, code: Byte, condition: Condition, register1: GeneralRegister, operand2: Shifter, destination: GeneralRegister)
    extends ConditionalARMOperation(condition) {

  override def encodeWord()(implicit page: MemoryPage) =
    (super.encodeWord() | (code << 21) | (register1.registerCode << 16) | (destination.registerCode << 12) | operand2.encode)

  override def toString = s"${mnemonic}${condition.mnemonicExtension} ${destination.toString}, ${register1.toString}, ${operand2.toString}"
}

class DataProcessingNoDestinationInstruction(mnemonic: String, code: Byte, condition: Condition, register1: GeneralRegister, operand2: Shifter)
    extends ConditionalARMOperation(condition) {

  override def encodeWord()(implicit page: MemoryPage) =
    (super.encodeWord() | (code << 21) | (register1.registerCode << 16) | operand2.encode)

  override def toString = s"${mnemonic}${condition.mnemonicExtension} ${register1.toString}, ${operand2.toString}"
}

class DataProcessingNoRegisterInstruction(mnemonic: String, code: Byte, condition: Condition, operand2: Shifter, destination: GeneralRegister)
    extends ConditionalARMOperation(condition) {
  override def encodeWord()(implicit page: MemoryPage) =
    (super.encodeWord() | (code << 21) | (destination.registerCode << 12) | operand2.encode)

  override def toString = s"${mnemonic}${condition.mnemonicExtension} ${destination.toString}, ${operand2.toString}"
}

class DataProcessing(val code: Byte)(implicit mnemonic: String)
    extends Operation(mnemonic) {

  def apply(register1: GeneralRegister, operand2: Shifter, destination: GeneralRegister, condition: Condition)(implicit processorMode: ProcessorMode): ARMOperation =
    new DataProcessingOperation(mnemonic, code, condition, register1, operand2, destination)

  def setFlags(register1: GeneralRegister, operand2: Shifter, destination: GeneralRegister, condition: Condition)(implicit processorMode: ProcessorMode): ARMOperation = {
    new DataProcessingOperation(mnemonic + "s", code, condition, register1, operand2, destination) {
      override def encodeWord()(implicit page: MemoryPage) =
        (super.encodeWord() | ARMOperation.sBit)
    }
  }

  def apply(register1: GeneralRegister, operand2: Shifter, condition: Condition)(implicit processorMode: ProcessorMode): ARMOperation = {
    new DataProcessingNoDestinationInstruction(mnemonic, code, condition, register1, operand2)
  }

  def apply(operand2: Shifter, destination: GeneralRegister, condition: Condition)(implicit processorMode: ProcessorMode): ARMOperation = {
    new DataProcessingNoRegisterInstruction(mnemonic, code, condition, operand2, destination)
  }

  def setFlags(operand2: Shifter, destination: GeneralRegister, condition: Condition)(implicit processorMode: ProcessorMode): ARMOperation = {
    new DataProcessingNoRegisterInstruction(mnemonic + "s", code, condition, operand2, destination) {
      override def encodeWord()(implicit page: MemoryPage) =
        (super.encodeWord() | ARMOperation.sBit)
    }
  }
}