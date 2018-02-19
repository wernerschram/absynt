package assembler.x86.operations

import assembler.x86.ProcessorMode

class Static(override val code: Seq[Byte], opcode: String)(implicit val processorMode: ProcessorMode) extends X86Operation {
  def mnemonic: String = opcode

  override def operands: Set[OperandInfo] = Set.empty
}