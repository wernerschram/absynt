package assembler.arm.operations

import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition.Condition
import assembler.arm.operands.registers.GeneralRegister
import assembler.memory.MemoryPage

class MultiplyOperation(val code: Byte, override val opcode: String, destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, val condition: Condition)
    extends Conditional {
  override def encodeWord()(implicit page: MemoryPage) =
    (super.encodeWord() | 0x00000090 | (code << 21) | (destination.registerCode << 16) | (source.registerCode << 8) | multiplyValue.registerCode)

  override def toString = s"${super.toString()} ${destination.toString}, ${multiplyValue.toString}, ${source.toString}"
}

class MultiplyWithRegisterInstruction(code: Byte, opcode: String, destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, addValue: GeneralRegister, condition: Condition)(implicit processorMode: ProcessorMode)
  extends MultiplyOperation(code, opcode, destination, source, multiplyValue, condition) {

  override def encodeWord()(implicit page: MemoryPage) =
    (super.encodeWord() | (addValue.registerCode << 12))

  override def toString = s"${super.toString()}, ${addValue.toString}"
}

class Multiply(val code: Byte)(implicit mnemonic: String)
    extends Operation(mnemonic) {

  def apply(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, condition: Condition)(implicit processorMode: ProcessorMode): ARMOperation =
    new MultiplyOperation(code, mnemonic, destination, source, multiplyValue, condition)

  def setFlags(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, condition: Condition)(implicit processorMode: ProcessorMode): ARMOperation = {
    new MultiplyOperation(code, mnemonic + "s", destination, source, multiplyValue, condition) {
      override def encodeWord()(implicit page: MemoryPage) = super.encodeWord() | ARMOperation.sBit
    }
  }

  def apply(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, addValue: GeneralRegister, condition: Condition)(implicit processorMode: ProcessorMode): ARMOperation =
    new MultiplyWithRegisterInstruction(code, mnemonic, destination, source, multiplyValue, addValue, condition)

  def setFlags(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, addValue: GeneralRegister, condition: Condition)(implicit processorMode: ProcessorMode): ARMOperation = {
    new MultiplyWithRegisterInstruction(code, mnemonic + "s", destination, source, multiplyValue, addValue, condition) {
      override def encodeWord()(implicit page: MemoryPage) = super.encodeWord() | ARMOperation.sBit
    }
  }
}