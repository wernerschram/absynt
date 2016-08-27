package assembler.arm.instructions
import assembler.ListExtensions._
import assembler.Encodable
import assembler.LabeledEncodable
import assembler.Label
import assembler.MemoryPage
import assembler.arm.operands.Condition._

abstract class ARMInstruction() extends Encodable[Int]() {
  def withLabel(label: Label): LabeledEncodable[Int] = ???
  override def size()(implicit page: MemoryPage) = 4

  def encodeWord()(implicit page: MemoryPage): Int

  override final def encode()(implicit page: MemoryPage): List[Int] =
    encodeWord :: Nil

  def encodeByte()(implicit page: MemoryPage): List[Byte] = encode.flatMap { x => x.encodeLittleEndian }
}

object ARMInstruction {
  val sBit = 0x00100000
}

abstract class ConditionalARMInstruction(val condition: Condition) extends ARMInstruction {

  override def encodeWord()(implicit page: MemoryPage): Int =
    (condition.value << 28)
}
