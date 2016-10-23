package assembler.arm.instructions

import assembler.Encodable
import assembler.Label
import assembler.LabeledEncodable
import assembler.ListExtensions._
import assembler.arm.operands.Condition.Condition
import assembler.memory.MemoryPage

trait ARMInstruction extends Encodable() {
  def withLabel(label: Label): LabeledEncodable = new LabeledARMInstruction(this, label)
  override def size()(implicit page: MemoryPage) = 4

  def encodeWord()(implicit page: MemoryPage): Int

  def encodeByte()(implicit page: MemoryPage): List[Byte] = encodeWord.encodeLittleEndian
}

object ARMInstruction {
  val sBit = 0x00100000
}

abstract class ConditionalARMInstruction(val condition: Condition) extends ARMInstruction {

  override def encodeWord()(implicit page: MemoryPage): Int =
    (condition.value << 28)
}


class LabeledARMInstruction(instruction: ARMInstruction, override val label: Label) extends ARMInstruction with LabeledEncodable {
  override def size()(implicit page: MemoryPage) = instruction.size()
  override def encodeByte()(implicit page: MemoryPage): List[Byte] = instruction.encodeByte()

  override def encodeWord()(implicit page: MemoryPage): Int = instruction.encodeWord()

  override def toString = s"$label: $instruction"

}