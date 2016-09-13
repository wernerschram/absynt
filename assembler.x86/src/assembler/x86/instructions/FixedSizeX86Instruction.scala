package assembler.x86.instructions

import assembler.Label
import assembler.LabeledEncodable
import assembler.MemoryPage

abstract class FixedSizeX86Instruction() extends X86Instruction() {
  def size()(implicit page: MemoryPage) = encodeByte().length
  def withLabel(label: Label): LabeledEncodable = new LabeledX86Instruction(this, label)
}

class LabeledX86Instruction(instruction: X86Instruction, override val label: Label) extends FixedSizeX86Instruction with LabeledEncodable {
  override def size()(implicit page: MemoryPage) = instruction.size()
  override def encodeByte()(implicit page: MemoryPage): List[Byte] = instruction.encodeByte()

  override def toString() = s"${label.toString}: ${instruction.toString()}"
}