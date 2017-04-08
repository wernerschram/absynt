package assembler.arm.instructions

import assembler.Label
import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition._
import assembler.arm.operations.{SoftwareInterrupt => SoftwareInterruptOpcode}

object SoftwareInterrupt {
  val opcode: String = "swi"

  def apply(interrupt: Int, condition: Condition = Always)(implicit processorMode: ProcessorMode, label: Label) =
    Immed(label, interrupt, condition)

  private def Immed(label: Label, interrupt: Int, condition: Condition) =
    new SoftwareInterruptOpcode(label, opcode, interrupt, condition)
}