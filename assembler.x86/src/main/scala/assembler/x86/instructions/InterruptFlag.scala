package assembler.x86.instructions

import assembler.x86.ProcessorMode
import assembler.x86.operations.Static

object ClearInterruptFlag {
  implicit val opcode = "cli"

  def apply()(implicit processorMode: ProcessorMode) =
    Static()

  private def Static()(implicit processorMode: ProcessorMode) = new Static(0xFA.toByte :: Nil, opcode)
}

object SetInterruptFlag {
  implicit val mnemonic = "sti"

  def apply()(implicit processorMode: ProcessorMode) =
    Static()

  private def Static()(implicit processorMode: ProcessorMode) = new Static(0xFB.toByte :: Nil, mnemonic)
}