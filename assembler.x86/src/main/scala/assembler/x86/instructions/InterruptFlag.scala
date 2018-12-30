package assembler.x86.instructions

import assembler.x86.ProcessorMode
import assembler.x86.operations.{NoDisplacement, NoImmediate, Static}

object ClearInterruptFlag {
  implicit val opcode: String = "cli"

  def apply()(implicit processorMode: ProcessorMode): Static =
    Static()

  private def Static()(implicit processorMode: ProcessorMode) = new Static(0xFA.toByte :: Nil, opcode) with NoDisplacement with NoImmediate
}

object SetInterruptFlag {
  implicit val mnemonic: String = "sti"

  def apply()(implicit processorMode: ProcessorMode): Static =
    Static()

  private def Static()(implicit processorMode: ProcessorMode) = new Static(0xFB.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate
}