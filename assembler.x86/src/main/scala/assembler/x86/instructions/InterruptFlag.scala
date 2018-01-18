package assembler.x86.instructions

import assembler.Label
import assembler.x86.ProcessorMode
import assembler.x86.operations.Static

object ClearInterruptFlag {
  implicit val opcode: String = "cli"

  def apply()(implicit processorMode: ProcessorMode): Static =
    Static()

  private def Static()(implicit processorMode: ProcessorMode) = new Static(0xFA.toByte :: Nil, opcode)
}

object SetInterruptFlag {
  implicit val mnemonic: String = "sti"

  def apply()(implicit processorMode: ProcessorMode): Static =
    Static()

  private def Static()(implicit processorMode: ProcessorMode) = new Static(0xFB.toByte :: Nil, mnemonic)
}