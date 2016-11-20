package assembler.x86.instructions.stack

import assembler.x86.ProcessorMode
import assembler.x86.operations.Static

object PushFlags {
  implicit val opcode = "pushf"

  def Static()(implicit processorMode: ProcessorMode) = new Static(0x9C.toByte :: Nil, opcode)

  def apply()(implicit processorMode: ProcessorMode) = Static()
}