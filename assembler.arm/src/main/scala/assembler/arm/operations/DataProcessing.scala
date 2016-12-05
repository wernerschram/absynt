package assembler.arm.operations

import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition.Condition
import assembler.arm.operands.Shifter
import assembler.arm.operands.registers.GeneralRegister
import assembler.memory.MemoryPage

class DataProcessingOperation(opcode: String, code: Byte, condition: Condition, register1: GeneralRegister, operand2: Shifter, destination: GeneralRegister)
    extends ConditionalARMOperation(condition) {
  override def mnemonic = opcode

  override def encodeWord()(implicit page: MemoryPage) =
    (super.encodeWord() | (code << 21) | (register1.registerCode << 16) | (destination.registerCode << 12) | operand2.encode)

  override def toString = s"${mnemonic}${condition.mnemonicExtension} ${destination.toString}, ${register1.toString}, ${operand2.toString}"
}

class DataProcessingNoDestinationInstruction(opcode: String, code: Byte, condition: Condition, register1: GeneralRegister, operand2: Shifter)
    extends ConditionalARMOperation(condition) {
  override def mnemonic = opcode

  override def encodeWord()(implicit page: MemoryPage) =
    (super.encodeWord() | (code << 21) | (register1.registerCode << 16) | operand2.encode)

  override def toString = s"${mnemonic}${condition.mnemonicExtension} ${register1.toString}, ${operand2.toString}"
}

class DataProcessingNoRegisterInstruction(opcode: String, code: Byte, condition: Condition, operand2: Shifter, destination: GeneralRegister)
    extends ConditionalARMOperation(condition) {
  override def mnemonic = opcode

  override def encodeWord()(implicit page: MemoryPage) =
    (super.encodeWord() | (code << 21) | (destination.registerCode << 12) | operand2.encode)

  override def toString = s"${mnemonic}${condition.mnemonicExtension} ${destination.toString}, ${operand2.toString}"
}
