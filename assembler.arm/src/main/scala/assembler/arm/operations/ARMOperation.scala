package assembler.arm.operations

import assembler.Encodable
import assembler.Label
import assembler.LabeledEncodable
import assembler.ListExtensions._
import assembler.arm.operands.Condition.Condition
import assembler.memory.MemoryPage

trait ARMOperation extends Encodable {
  def withLabel(label: Label): LabeledEncodable = new LabeledARMOperation(this, label)
  override def size()(implicit page: MemoryPage) = 4

  val opcode: String

  def mnemonic: List[PartialName] = PartialName(opcode, 0) :: Nil

  def encodeWord()(implicit page: MemoryPage): Int = 0

  def encodeByte()(implicit page: MemoryPage): List[Byte] = encodeWord.encodeLittleEndian

  override def toString = mnemonic.sortBy { part => part.order }.map { part => part.name }.mkString
}

object ARMOperation {
  val sBit = 0x00100000
}

case class PartialName(val name: String, val order: Int)

trait Conditional extends ARMOperation {
  self: ARMOperation =>

  override def mnemonic = PartialName(condition.mnemonicExtension, 3) :: super.mnemonic

  val condition: Condition

  override def encodeWord()(implicit page: MemoryPage): Int =
    super.encodeWord() | (condition.value << 28)
}

class LabeledARMOperation(instruction: ARMOperation, override val label: Label) extends ARMOperation with LabeledEncodable {
  val opcode = instruction.opcode

  override def size()(implicit page: MemoryPage) = instruction.size()
  override def encodeByte()(implicit page: MemoryPage): List[Byte] = instruction.encodeByte()

  override def encodeWord()(implicit page: MemoryPage): Int = instruction.encodeWord()

  override def toString = s"$label: $instruction"

}