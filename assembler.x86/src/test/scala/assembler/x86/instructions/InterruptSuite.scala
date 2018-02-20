package assembler.x86.instructions

import assembler.Hex
import assembler.x86.ProcessorMode
import assembler.x86.operands.ImmediateValue._
import org.scalatest.{Matchers, WordSpec}

class InterruptSuite extends WordSpec with Matchers {

  "an Interrupt instruction" when {

    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode int 0x03" in {
        Interrupt(0x03.toByte).encodeByte should be (Hex.lsb("CC"))
      }

      "correctly represent int 0x03 as a string" in {
        Interrupt(0x03.toByte).toString should be("int 3")
      }
    }
    "in long mode" should {

      import ProcessorMode.Long._

      "correctly encode int 0x01" in {
        Interrupt(0x01.toByte).encodeByte should be (Hex.lsb("CD 01"))
      }

      "correctly represent int 0x01 as a string" in {
        Interrupt(0x01.toByte).toString should be("int 1")
      }
    }
  }
}