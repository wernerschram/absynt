package assembler.arm.instructions

import assembler.arm.operands.Condition._
import assembler.arm.operations.{SoftwareInterrupt => SoftwareInterruptOpcode}

object SoftwareInterrupt {
  val opcode: String = "swi"
  private def Immed(interrupt: Int, condition: Condition) =
    new SoftwareInterruptOpcode(opcode, interrupt, condition)

    def apply(interrupt: Int, condition: Condition = Always) =
      Immed(interrupt, condition)
}