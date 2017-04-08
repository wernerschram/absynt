package assembler.x86.instructions

import assembler.Label
import assembler.x86.ProcessorMode
import assembler.x86.operations.Static

object ClearInterruptFlag {
  implicit val opcode = "cli"

  def apply()(implicit label: Label, processorMode: ProcessorMode) =
    Static()

  private def Static()(implicit label: Label, processorMode: ProcessorMode) = new Static(label, 0xFA.toByte :: Nil, opcode)
}

object SetInterruptFlag {
  implicit val mnemonic = "sti"

  def apply()(implicit label: Label, processorMode: ProcessorMode) =
    Static()

  private def Static()(implicit label: Label, processorMode: ProcessorMode) = new Static(label, 0xFB.toByte :: Nil, mnemonic)
}