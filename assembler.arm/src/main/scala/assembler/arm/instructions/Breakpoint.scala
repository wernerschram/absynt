package assembler.arm.instructions

import assembler.Label
import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition._
import assembler.arm.operations.Miscellaneous

object Breakpoint {
  val code: Byte = 0x09
  val opcode: String = "bkpt"

  def apply(value: Short, condition: Condition = Always)(implicit processorMode: ProcessorMode, label: Label): Miscellaneous =
    Immed(label, value, condition)

  private def Immed(label: Label, value: Short, condition: Condition) = new Miscellaneous(label, code, opcode, value, condition)
}