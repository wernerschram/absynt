package assembler.x86.operations

import assembler.x86.ProcessorMode

class Static(override val code: List[Byte], implicit val mnemonic: String)(override implicit val processorMode: ProcessorMode) extends X86Operation {
  def operands: List[assembler.x86.operands.Operand] = Nil
}