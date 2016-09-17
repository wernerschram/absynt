package assembler.arm.opcodes

import assembler.memory.MemoryPage
import assembler.arm.ProcessorMode
import assembler.arm.instructions.ARMInstruction
import assembler.arm.instructions.ConditionalARMInstruction
import assembler.arm.operands.Condition._
import assembler.arm.operands.registers.GeneralRegister

class MultiplyInstruction(val code: Byte, mnemonic: String, destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, condition: Condition)(implicit processorMode: ProcessorMode)
    extends ConditionalARMInstruction(condition) {
  override def encodeWord()(implicit page: MemoryPage) =
    (super.encodeWord() | 0x00000090 | (code << 21) | (destination.registerCode << 16) | (source.registerCode << 8) | multiplyValue.registerCode)

  override def toString() = s"${mnemonic}${condition.mnemonicExtension} ${destination.toString}, ${source.toString}, ${multiplyValue.toString}"
}

class MultiplyWithRegisterInstruction(code: Byte, mnemonic: String, destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, addValue: GeneralRegister, condition: Condition)(implicit processorMode: ProcessorMode) 
  extends MultiplyInstruction(code, mnemonic, destination, source, multiplyValue, condition) {
  
  override def encodeWord()(implicit page: MemoryPage) =
    (super.encodeWord() | (addValue.registerCode << 12))

  override def toString() = s"${mnemonic}${condition.mnemonicExtension} ${destination.toString}, ${source.toString}, ${multiplyValue.toString}, ${addValue.toString}"
}

class Multiply(val code: Byte)(implicit mnemonic: String)
    extends Opcode(mnemonic) {

  def apply(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, condition: Condition)(implicit processorMode: ProcessorMode): ARMInstruction =
    new MultiplyInstruction(code, mnemonic, destination, source, multiplyValue, condition)

  def setFlags(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, condition: Condition)(implicit processorMode: ProcessorMode): ARMInstruction = {
    new MultiplyInstruction(code, mnemonic + "s", destination, source, multiplyValue, condition) {
      override def encodeWord()(implicit page: MemoryPage) = super.encodeWord() | ARMInstruction.sBit
    }
  }
  
  def apply(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, addValue: GeneralRegister, condition: Condition)(implicit processorMode: ProcessorMode): ARMInstruction =
    new MultiplyWithRegisterInstruction(code, mnemonic, destination, source, multiplyValue, addValue, condition)

  def setFlags(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, addValue: GeneralRegister, condition: Condition)(implicit processorMode: ProcessorMode): ARMInstruction = {
    new MultiplyWithRegisterInstruction(code, mnemonic + "s", destination, source, multiplyValue, addValue, condition) {
      override def encodeWord()(implicit page: MemoryPage) = super.encodeWord() | ARMInstruction.sBit
    }
  }
}