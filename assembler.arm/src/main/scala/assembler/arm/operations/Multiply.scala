package assembler.arm.operations

import assembler.Label
import assembler.arm.operands.Condition.Condition
import assembler.arm.operands.registers.GeneralRegister
import assembler.sections.Section

class MultiplyOperation(val label: Label, val code: Byte, override val opcode: String, destination: GeneralRegister,
                        source: GeneralRegister, multiplyValue: GeneralRegister, val condition: Condition)
  extends Conditional {
  override def encodeWord()(implicit page: Section): Int =
    super.encodeWord() |
      0x00000090 |
      (code << 21) |
      (destination.registerCode << 16) |
      (source.registerCode << 8) |
      multiplyValue.registerCode

  override def toString = s"$labelPrefix$mnemonicString ${destination.toString}, ${multiplyValue.toString}, ${source.toString}"
}

class MultiplyWithRegisterOperation(label: Label, code: Byte, opcode: String, destination: GeneralRegister, source: GeneralRegister,
                                    multiplyValue: GeneralRegister, addValue: GeneralRegister, condition: Condition)
  extends MultiplyOperation(label, code, opcode, destination, source, multiplyValue, condition) {

  override def encodeWord()(implicit page: Section): Int =
    super.encodeWord() | (addValue.registerCode << 12)

  override def toString = s"${super.toString()}, ${addValue.toString}"
}
