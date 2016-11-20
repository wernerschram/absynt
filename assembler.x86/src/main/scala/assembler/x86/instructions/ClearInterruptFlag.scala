package assembler.x86.instructions

import assembler.x86.ProcessorMode
import assembler.x86.operations.Static

object ClearInterruptFlag {
  implicit val opcode = "cli"

  private def Static()(implicit processorMode: ProcessorMode) = new Static(0xFA.toByte :: Nil, opcode)

  def apply()(implicit processorMode: ProcessorMode) =
    Static()
}