package assembler.x86.operations

import assembler.x86.ProcessorMode

abstract class Static(override val code: Seq[Byte], opcode: String)(override implicit val processorMode: ProcessorMode)
  extends X86Operation(code) with NoModRM {

  self: X86Operation with DisplacementBytes with ImmediateBytes =>
  def mnemonic: String = opcode
}