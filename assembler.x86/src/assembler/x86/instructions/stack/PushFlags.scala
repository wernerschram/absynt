package assembler.x86.instructions.stack

import assembler.x86.ProcessorMode
import assembler.x86.opcodes.Static

object PushFlags {
  implicit val opcode = "pushf"

  case object Static extends Static(0x9C.toByte :: Nil)

  def apply()(implicit processorMode: ProcessorMode) = Static()
}