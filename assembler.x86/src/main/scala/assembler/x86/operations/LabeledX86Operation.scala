package assembler.x86.operations

import assembler.Label
import assembler.LabeledEncodable
import assembler.memory.MemoryPage

class LabeledX86Operation(instruction: X86Operation, override val label: Label) extends X86Operation with LabeledEncodable {
  override def size()(implicit page: MemoryPage) = instruction.size()
  override def encodeByte()(implicit page: MemoryPage): List[Byte] = instruction.encodeByte()

  override def withLabel(label: Label): LabeledEncodable = new LabeledX86Operation(this, label)

  def code: List[Byte] = instruction.code
  val mnemonic: String = instruction.mnemonic
  def operands: List[assembler.x86.operands.Operand] = instruction.operands
  implicit val processorMode: assembler.x86.ProcessorMode = instruction.processorMode

  override def toString() = s"${label.toString}: ${instruction.toString()}"
}
