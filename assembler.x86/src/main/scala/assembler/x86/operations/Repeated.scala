package assembler.x86.operations

import assembler.memory.MemoryPage
import assembler.x86.instructions.FixedSizeX86Operation

trait Repeated extends FixedSizeX86Operation {

  self: FixedSizeX86Operation =>

  abstract override def encodeByte()(implicit page: MemoryPage): List[Byte] = {
    0xF3.toByte :: super.encodeByte()
  }

}
