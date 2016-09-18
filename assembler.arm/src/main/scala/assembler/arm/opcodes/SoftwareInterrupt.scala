package assembler.arm.opcodes

import assembler.arm.ProcessorMode
import assembler.arm.instructions.ARMInstruction
import assembler.arm.operands.Condition.Condition
import assembler.memory.MemoryPage

class SoftwareInterrupt()(implicit mnemonic: String)
    extends Opcode(mnemonic) {

  def apply(interrupt: Int, condition: Condition)(implicit processorMode: ProcessorMode): ARMInstruction = {
    new ARMInstruction() {
      override def encodeWord()(implicit page: MemoryPage) = ((condition.value << 28) | 0x0f000000 | (interrupt) )
      
      override def toString() = s"${mnemonic} ${interrupt.toString()}"
    }
  }
}