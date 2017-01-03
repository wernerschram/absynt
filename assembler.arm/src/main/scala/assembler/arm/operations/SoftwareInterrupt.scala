package assembler.arm.operations

import assembler.arm.operands.Condition.Condition
import assembler.memory.MemoryPage

class SoftwareInterrupt(override val opcode: String, interrupt: Int, condition: Condition)
  extends ARMOperation() {
  override def encodeWord()(implicit page: MemoryPage): Int = (condition.value << 28) | 0x0f000000 | interrupt

  override def toString = s"${super.toString} ${interrupt.toString}"
}
