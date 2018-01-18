package assembler.arm.operations

import assembler.ListExtensions._
import assembler.arm.operands.Condition.Condition
import assembler.resource.Encodable

trait NamedOperation {
  val opcode: String

  def mnemonic: List[PartialName] = PartialName(opcode, 0) :: Nil

  lazy val mnemonicString: String = mnemonic.sortBy { part => part.order }.map { part => part.name }.mkString

}

abstract class ARMOperation extends Encodable with NamedOperation {
  override def size = 4

  override def encodeByte: Seq[Byte] = encodeWord.encodeLittleEndian

  def encodeWord: Int = 0

  override def toString: String = s"$mnemonicString"
}

object ARMOperation {
  val sBit = 0x00100000
}

case class PartialName(name: String, order: Int)

trait NamedConditional extends NamedOperation {
  val condition: Condition

  override def mnemonic: List[PartialName] = PartialName(condition.mnemonicExtension, 3) :: super.mnemonic
}

abstract class Conditional extends ARMOperation with NamedConditional {
  override def encodeWord: Int =
    super.encodeWord | (condition.value << 28)
}
