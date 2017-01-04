package assembler.x86.operations

import assembler.sections.Section

trait Repeated extends X86Operation {
  self: X86Operation =>

  abstract override def encodeByte()(implicit page: Section): List[Byte] = {
    0xF3.toByte :: super.encodeByte()
  }

  abstract override def mnemonic = s"rep ${super.mnemonic}"
}
