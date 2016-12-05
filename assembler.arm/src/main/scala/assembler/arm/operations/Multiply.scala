package assembler.arm.operations

import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition.Condition
import assembler.arm.operands.registers.GeneralRegister
import assembler.memory.MemoryPage

class MultiplyOperation(val code: Byte, override val mnemonic: String, destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, condition: Condition)(implicit processorMode: ProcessorMode)
    extends ConditionalARMOperation(condition) {
  override def encodeWord()(implicit page: MemoryPage) =
    (super.encodeWord() | 0x00000090 | (code << 21) | (destination.registerCode << 16) | (source.registerCode << 8) | multiplyValue.registerCode)

  override def toString = s"${mnemonic}${condition.mnemonicExtension} ${destination.toString}, ${multiplyValue.toString}, ${source.toString}"
}

class MultiplyWithRegisterInstruction(code: Byte, mnemonic: String, destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, addValue: GeneralRegister, condition: Condition)(implicit processorMode: ProcessorMode)
  extends MultiplyOperation(code, mnemonic, destination, source, multiplyValue, condition) {

  override def encodeWord()(implicit page: MemoryPage) =
    (super.encodeWord() | (addValue.registerCode << 12))

  override def toString = s"${mnemonic}${condition.mnemonicExtension} ${destination.toString}, ${multiplyValue.toString}, ${source.toString}, ${addValue.toString}"
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