package assembler.x86.instructions

import assembler.Encodable
import assembler.Label
import assembler.LabeledEncodable
import assembler.MemoryPage

abstract class X86Instruction() extends Encodable[Byte]() {
  def withLabel(label: Label): LabeledEncodable[Byte]
  
  def encodeByte()(implicit page: MemoryPage): List[Byte] = encode
}