package assembler.arm.instructions

import assembler.{Encodable, Hex}
import assembler.arm.ProcessorMode
import assembler.arm.operations.ARMOperation
import assembler.sections.Section
import org.scalatest.{Matchers, WordSpec}

class SoftwareInterruptSuite extends WordSpec with Matchers {

  implicit val page: Section = Section(List.empty[Encodable], 0)

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