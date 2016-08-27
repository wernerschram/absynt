package assembler.arm.instructions.dataprocessing

import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec
import assembler.MemoryPage
import assembler.Hex
import assembler.arm.ProcessorMode
import assembler.arm.operands.registers.GeneralRegister._
import assembler.arm.instructions.ARMInstruction
import assembler.arm.operands.Condition._
import assembler.arm.operands.Shifter
import assembler.arm.operands.Condition
import assembler.arm.instructions.Breakpoint

class MiscellaneousSuite extends WordSpec with ShouldMatchers {

  implicit val page: MemoryPage = new MemoryPage(List.empty[ARMInstruction])

  "a Breakpoint instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode bkpt 0x00cc" in {
        Breakpoint(0xf15.toShort).encode should be(Hex.MSB("e120f175"))
      }
      
    }
  }
}