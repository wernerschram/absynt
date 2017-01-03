package assembler.arm.instructions

import assembler.Hex
import assembler.arm.ProcessorMode
import assembler.arm.operations.ARMOperation
import assembler.memory.MemoryPage
import org.scalatest.{Matchers, WordSpec}

class SoftwareInterruptSuite extends WordSpec with Matchers {

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