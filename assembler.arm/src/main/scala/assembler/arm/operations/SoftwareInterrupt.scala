package assembler.arm.operations

import assembler.Label
import assembler.arm.operands.Condition.Condition
import assembler.sections.Section

class SoftwareInterrupt(label: Label, override val opcode: String, interrupt: Int, condition: Condition)
  extends ARMOperation(label) {
  override def encodeWord: Int = (condition.value << 28) | 0x0f000000 | interrupt

  override def toString = s"$labelPrefix$mnemonicString ${interrupt.toString}"
}
