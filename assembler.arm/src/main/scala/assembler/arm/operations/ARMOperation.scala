package assembler.arm.operations

import assembler.ListExtensions._
import assembler.arm.operands.Condition.Condition
import assembler.sections.Section
import assembler.Encodable

trait ARMOperation extends Encodable {
  self: Encodable =>
  val opcode: String

  override def size()(implicit page: Section) = 4

  def encodeByte()(implicit page: Section): Seq[Byte] = encodeWord.encodeLittleEndian

  def encodeWord()(implicit page: Section): Int = 0

  override def toString: String = s"$labelPrefix$mnemonicString"

  def mnemonic: List[PartialName] = PartialName(opcode, 0) :: Nil

  lazy val mnemonicString: String = mnemonic.sortBy { part => part.order }.map { part => part.name }.mkString
}

object ARMOperation {
  val sBit = 0x00100000
}

case class PartialName(name: String, order: Int)

trait Conditional extends ARMOperation {
  self: ARMOperation =>

  val condition: Condition

  override def mnemonic: List[PartialName] = PartialName(condition.mnemonicExtension, 3) :: super.mnemonic

  override def encodeWord()(implicit page: Section): Int =
    super.encodeWord() | (condition.value << 28)
}
