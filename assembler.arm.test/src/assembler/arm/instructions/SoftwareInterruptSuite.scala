package assembler.arm.instructions.dataprocessing

import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec
import assembler.memory.MemoryPage
import assembler.Hex
import assembler.arm.ProcessorMode
import assembler.arm.operands.registers.GeneralRegister._
import assembler.arm.instructions.ARMInstruction
import assembler.arm.operands.Condition._
import assembler.arm.operands.Shifter
import assembler.arm.operands.Condition
import assembler.arm.instructions.SoftwareInterrupt

class SoftwareInterruptSuite extends WordSpec with ShouldMatchers {

  implicit val page: MemoryPage = new MemoryPage(List.empty[ARMInstruction])

  "a SoftwareInterrupt instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode swi 0x0000000a" in {
        SoftwareInterrupt(10).encodeByte should be(Hex.MSB("ef00000a"))
      }
      
    }
  }
}