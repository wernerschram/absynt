package assembler.arm.operations

import assembler.ListExtensions._
import assembler.arm.operands.Condition.Condition
import assembler.memory.MemoryPage
import assembler.{Encodable, Label, LabeledEncodable}

trait ARMOperation extends Encodable {
  val opcode: String

  def withLabel(label: Label): LabeledEncodable = new LabeledARMOperation(this, label)

  override def size()(implicit page: MemoryPage) = 4

  def encodeByte()(implicit page: MemoryPage): List[Byte] = encodeWord.encodeLittleEndian

  def encodeWord()(implicit page: MemoryPage): Int = 0

  override def toString: String = mnemonic.sortBy { part => part.order }.map { part => part.name }.mkString

  def mnemonic: List[PartialName] = PartialName(opcode, 0) :: Nil
}

object ARMOperation {
  val sBit = 0x00100000
}

case class PartialName(name: String, order: Int)

trait Conditional extends ARMOperation {
  self: ARMOperation =>

  val condition: Condition

  override def mnemonic: List[PartialName] = PartialName(condition.mnemonicExtension, 3) :: super.mnemonic

  override def encodeWord()(implicit page: MemoryPage): Int =
    super.encodeWord() | (condition.value << 28)
}

class LabeledARMOperation(override val value: ARMOperation, override val label: Label) extends ARMOperation with LabeledEncodable {
  val opcode: String = value.opcode

  override def encodeWord()(implicit page: MemoryPage): Int = value.encodeWord()
}