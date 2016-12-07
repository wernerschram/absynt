package assembler.arm.operations

import assembler.Encodable
import assembler.Label
import assembler.LabeledEncodable
import assembler.ListExtensions._
import assembler.arm.operands.Condition.Condition
import assembler.memory.MemoryPage

trait ARMOperation extends Encodable() {
  def withLabel(label: Label): LabeledEncodable = new LabeledARMOperation(this, label)
  override def size()(implicit page: MemoryPage) = 4

  def mnemonic: String

  def encodeWord()(implicit page: MemoryPage): Int = 0

  def encodeByte()(implicit page: MemoryPage): List[Byte] = encodeWord.encodeLittleEndian
}

object ARMOperation {
  val sBit = 0x00100000
}

trait Conditional extends ARMOperation {
  self: ARMOperation =>

  val condition: Condition

  override def encodeWord()(implicit page: MemoryPage): Int =
    super.encodeWord() | (condition.value << 28)
}

class LabeledARMOperation(instruction: ARMOperation, override val label: Label) extends ARMOperation with LabeledEncodable {
  def mnemonic = instruction.mnemonic

  override def size()(implicit page: MemoryPage) = instruction.size()
  override def encodeByte()(implicit page: MemoryPage): List[Byte] = instruction.encodeByte()

  override def encodeWord()(implicit page: MemoryPage): Int = instruction.encodeWord()

  override def toString = s"$label: $instruction"

}