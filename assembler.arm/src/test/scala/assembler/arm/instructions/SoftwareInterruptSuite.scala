package assembler.arm.instructions

import assembler.Hex
import assembler.arm.ProcessorMode
import org.scalatest.{Matchers, WordSpec}

class SoftwareInterruptSuite extends WordSpec with Matchers {

  "a SoftwareInterrupt instruction" when {
    "in a32 mode" should {

      import ProcessorMode.A32._

      "correctly encode swi 10" in {
        SoftwareInterrupt(10).encodeByte should be(Hex.msb("ef00000a"))
      }

      "correctly represent swi 10 as a string" in {
        SoftwareInterrupt(10).toString should be("swi 10")
      }
    }
  }
}