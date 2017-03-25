package assembler.x86.instructions

import assembler.{Designation, Encodable, Hex}
import assembler.sections.Section
import assembler.x86.ProcessorMode
import assembler.x86.operations.X86Operation
import org.scalatest.{Matchers, WordSpec}

class InterruptFlagSuite extends WordSpec with Matchers {

  implicit val page: Section = Section(List.empty[Designation[Encodable]])

  "a ClearInterruptFlag instruction" when {

    "in real mode" should {

      implicit val processorMode = ProcessorMode.Real

      "correctly encode cli" in {
        ClearInterruptFlag().encodeByte should be(Hex.lsb("FA"))
      }
    }

    "in long mode" should {

      implicit val processorMode = ProcessorMode.Long

      "correctly encode cli" in {
        ClearInterruptFlag().encodeByte should be(Hex.lsb("FA"))
      }
    }
  }

  "a SetInterruptFlag instruction" when {

    "in real mode" should {

      implicit val processorMode = ProcessorMode.Real

      "correctly encode sti" in {
        SetInterruptFlag().encodeByte should be(Hex.lsb("FB"))
      }
    }
    "in long mode" should {

      implicit val processorMode = ProcessorMode.Long


      "correctly encode sti" in {
        SetInterruptFlag().encodeByte should be(Hex.lsb("FB"))
      }
    }
  }
}
