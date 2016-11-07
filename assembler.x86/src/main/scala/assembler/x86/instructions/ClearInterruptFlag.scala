package assembler.x86.instructions

import assembler.x86.ProcessorMode
import assembler.x86.opcodes.Static

object ClearInterruptFlag {
  implicit val opcode = "cli"

  private val Static = new Static(0xFA.toByte :: Nil)

  def apply()(implicit processorMode: ProcessorMode) =
    Static()
}