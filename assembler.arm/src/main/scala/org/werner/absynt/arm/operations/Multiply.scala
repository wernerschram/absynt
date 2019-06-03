package org.werner.absynt.arm.operations

import org.werner.absynt.arm.operands.Condition.Condition
import org.werner.absynt.arm.operands.registers.GeneralRegister

class MultiplyOperation(val code: Byte, override val opcode: String, destination: GeneralRegister,
                        source: GeneralRegister, multiplyValue: GeneralRegister, val condition: Condition)
  extends Conditional {
  override def encodeWord: Int =
    super.encodeWord |
      0x00000090 |
      (code << 21) |
      (destination.registerCode << 16) |
      (source.registerCode << 8) |
      multiplyValue.registerCode

  override def toString = s"$mnemonicString ${destination.toString}, ${multiplyValue.toString}, ${source.toString}"
}

class MultiplyWithRegisterOperation(code: Byte, opcode: String, destination: GeneralRegister, source: GeneralRegister,
                                    multiplyValue: GeneralRegister, addValue: GeneralRegister, condition: Condition)
  extends MultiplyOperation(code, opcode, destination, source, multiplyValue, condition) {

  override def encodeWord: Int =
    super.encodeWord | (addValue.registerCode << 12)

  override def toString = s"${super.toString()}, ${addValue.toString}"
}
