package assembler.x86.operations

import assembler.memory.MemoryPage

trait Repeated extends X86Operation {

  self: X86Operation =>

  abstract override def encodeByte()(implicit page: MemoryPage): List[Byte] = {
    0xF3.toByte :: super.encodeByte()
  }

}
