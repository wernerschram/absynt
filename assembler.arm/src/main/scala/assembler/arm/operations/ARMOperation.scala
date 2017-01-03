package assembler.arm.operations

import assembler.{Encodable, Label, LabeledEncodable}
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

  override def toString: String = mnemonic.sortBy { part => part.order }.map { part => part.name }.mkString
}

object ARMOperation {
  val sBit = 0x00100000
}

case class PartialName(name: String, order: Int)

trait Conditional extends ARMOperation {
  self: ARMOperation =>

  override def mnemonic: List[PartialName] = PartialName(condition.mnemonicExtension, 3) :: super.mnemonic

  val condition: Condition

  override def encodeWord()(implicit page: MemoryPage): Int =
    super.encodeWord() | (condition.value << 28)
}

class LabeledARMOperation(override val value: ARMOperation, override val label: Label) extends ARMOperation with LabeledEncodable {
  val opcode: String = value.opcode
  
  override def encodeWord()(implicit page: MemoryPage): Int = value.encodeWord()
}