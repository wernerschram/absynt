package assembler.arm.operations

import assembler.arm.operands.Condition.Condition

class SoftwareInterrupt(override val opcode: String, interrupt: Int, condition: Condition)
  extends ARMOperation {
  override def encodeWord: Int = (condition.value << 28) | 0x0f000000 | interrupt

  override def toString = s"$mnemonicString ${interrupt.toString}"
}
