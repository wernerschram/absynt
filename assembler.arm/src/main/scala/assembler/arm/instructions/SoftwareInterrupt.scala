package assembler.arm.instructions

import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition._
import assembler.arm.operations.{SoftwareInterrupt => SoftwareInterruptOpcode}

object SoftwareInterrupt {
  val opcode: String = "swi"

  def apply(interrupt: Int, condition: Condition = Always)(implicit processorMode: ProcessorMode): SoftwareInterruptOpcode =
    Immed(interrupt, condition)

  private def Immed(interrupt: Int, condition: Condition) =
    new SoftwareInterruptOpcode(opcode, interrupt, condition)
}