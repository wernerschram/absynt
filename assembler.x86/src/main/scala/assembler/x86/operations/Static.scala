package assembler.x86.operations

import assembler.Label
import assembler.x86.ProcessorMode

class Static(val label: Label, override val code: List[Byte], opcode: String)(implicit val processorMode: ProcessorMode) extends X86Operation {
  def mnemonic: String = opcode

  def operands: List[assembler.x86.operands.Operand] = Nil
}