package assembler.x86.instructions

import assembler.Hex
import assembler.sections.Section
import assembler.x86.ProcessorMode
import assembler.x86.operands.ImmediateValue._
import assembler.x86.operations.X86Operation
import org.scalatest.{Matchers, WordSpec}

class InterruptSuite extends WordSpec with Matchers {

  implicit val page: Section = new Section(List.empty[X86Operation])

  "an Interrupt instruction" when {

    "in real mode" should {

      implicit val processorMode = ProcessorMode.Real

      "correctly encode int 0x03" in {
        Interrupt(0x03.toByte).encodeByte should be (Hex.lsb("CC"))
      }

      "correctly represent int 0x03 as a string" in {
        Interrupt(0x03.toByte).toString should be("int 3")
      }
    }
    "in long mode" should {

      implicit val processorMode = ProcessorMode.Long

      "correctly encode int 0x01" in {
        Interrupt(0x01.toByte).encodeByte should be (Hex.lsb("CD 01"))
      }

      "correctly represent int 0x01 as a string" in {
        Interrupt(0x01.toByte).toString should be("int 1")
      }

      "throw an AssertionError for INT 0x0001" in {
        an [AssertionError] should be thrownBy {
          Interrupt(0x01.toShort).encodeByte()
        }
      }
    }
  }
}