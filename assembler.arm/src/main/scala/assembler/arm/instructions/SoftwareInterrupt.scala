package assembler.arm.instructions

import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition._
import assembler.arm.operations.{ SoftwareInterrupt => SoftwareInterruptOpcode }

object SoftwareInterrupt {
  val opcode: String = "swi"
  private val Immed = new SoftwareInterruptOpcode()(opcode)

    def apply(interrupt: Int, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
      Immed(interrupt, condition)
}