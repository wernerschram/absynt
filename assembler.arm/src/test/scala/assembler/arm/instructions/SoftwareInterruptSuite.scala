package assembler.arm.instructions.dataprocessing

import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec

import assembler.Hex
import assembler.arm.ProcessorMode
import assembler.arm.operations.ARMOperation
import assembler.arm.instructions.SoftwareInterrupt
import assembler.memory.MemoryPage

class SoftwareInterruptSuite extends WordSpec with ShouldMatchers {

  implicit val page: MemoryPage = new MemoryPage(List.empty[ARMOperation])

  "a SoftwareInterrupt instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode swi 10" in {
        SoftwareInterrupt(10).encodeByte should be(Hex.msb("ef00000a"))
      }

      "correctly represent swi 10 as a string" in {
        SoftwareInterrupt(10).toString should be("swi 10")
      }
    }
  }
}