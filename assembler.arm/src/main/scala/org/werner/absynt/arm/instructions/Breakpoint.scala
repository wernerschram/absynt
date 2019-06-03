package org.werner.absynt.arm.instructions

import org.werner.absynt.arm.ProcessorMode
import org.werner.absynt.arm.operands.Condition._
import org.werner.absynt.arm.operations.Miscellaneous

object Breakpoint {
  val code: Byte = 0x09
  val opcode: String = "bkpt"

  def apply(value: Short, condition: Condition = Always)(implicit processorMode: ProcessorMode): Miscellaneous =
    Immed(value, condition)

  private def Immed(value: Short, condition: Condition) = new Miscellaneous(code, opcode, value, condition)
}