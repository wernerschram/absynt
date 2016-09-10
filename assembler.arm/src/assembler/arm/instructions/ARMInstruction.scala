package assembler.arm.instructions
import assembler.ListExtensions._
import assembler.Encodable
import assembler.LabeledEncodable
import assembler.Label
import assembler.MemoryPage
import assembler.arm.operands.Condition._

abstract class ARMInstruction() extends Encodable() {
  def withLabel(label: Label): LabeledEncodable = ???
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
