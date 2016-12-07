package assembler.arm.operations

import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition.Condition
import assembler.memory.MemoryPage

class SoftwareInterrupt()(implicit mnemonic: String)
    extends Operation(mnemonic) {

  def apply(interrupt: Int, condition: Condition)(implicit processorMode: ProcessorMode): ARMOperation = {
    new ARMOperation() {
      val opcode = SoftwareInterrupt.this.mnemonic

      override def encodeWord()(implicit page: MemoryPage) = ((condition.value << 28) | 0x0f000000 | (interrupt) )

      override def toString = s"${super.toString()} ${interrupt.toString()}"
    }
  }
}