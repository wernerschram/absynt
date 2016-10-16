package assembler.arm.instructions.dataprocessing

import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec

import assembler.Hex
import assembler.arm.ProcessorMode
import assembler.arm.instructions.ARMInstruction
import assembler.arm.instructions.SoftwareInterrupt
import assembler.memory.MemoryPage

class SoftwareInterruptSuite extends WordSpec with ShouldMatchers {

  implicit val page: MemoryPage = new MemoryPage(List.empty[ARMInstruction])

  "a SoftwareInterrupt instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode swi 0x0000000a" in {
        SoftwareInterrupt(10).encodeByte should be(Hex.msb("ef00000a"))
      }

    }
  }
}