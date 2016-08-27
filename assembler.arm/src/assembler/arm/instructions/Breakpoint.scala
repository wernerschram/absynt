package assembler.arm.instructions

import assembler.arm.operands.Condition._
import assembler.arm.ProcessorMode
import assembler.arm.opcodes.Miscellaneous

object Breakpoint {
  val code: Byte = 0x09
  val opcode: String = "bkpt"
  private val Immed = new Miscellaneous(code)(opcode)

    def apply(value: Short, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
      Immed(value, condition)
}