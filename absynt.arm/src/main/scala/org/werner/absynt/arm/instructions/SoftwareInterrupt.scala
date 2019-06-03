package org.werner.absynt.arm.instructions

import org.werner.absynt.arm.ProcessorMode
import org.werner.absynt.arm.operands.Condition._
import org.werner.absynt.arm.operations.{SoftwareInterrupt => SoftwareInterruptOpcode}

object SoftwareInterrupt {
  val opcode: String = "swi"

  def apply(interrupt: Int, condition: Condition = Always)(implicit processorMode: ProcessorMode): SoftwareInterruptOpcode =
    Immed(interrupt, condition)

  private def Immed(interrupt: Int, condition: Condition) =
    new SoftwareInterruptOpcode(opcode, interrupt, condition)
}