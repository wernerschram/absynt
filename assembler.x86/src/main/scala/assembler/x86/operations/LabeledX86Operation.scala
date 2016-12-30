package assembler.x86.operations

import assembler.Label
import assembler.LabeledEncodable
import assembler.memory.MemoryPage

class LabeledX86Operation(override val value: X86Operation, override val label: Label) extends X86Operation with LabeledEncodable {
  def code: List[Byte] = value.code
  val mnemonic: String = value.mnemonic
  def operands: List[assembler.x86.operands.Operand] = value.operands
  implicit val processorMode: assembler.x86.ProcessorMode = value.processorMode
}
