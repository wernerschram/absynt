package assembler.x86.instructions.interrupt

import org.scalamock.scalatest.MockFactory
import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec

import assembler.Hex
import assembler.memory.MemoryPage
import assembler.x86.ProcessorMode
import assembler.x86.instructions.ClearInterruptFlag
import assembler.x86.instructions.FixedSizeX86Operation2

class ClearInterruptFlagSuite extends WordSpec with ShouldMatchers with MockFactory {

  implicit val page: MemoryPage = new MemoryPage(List.empty[FixedSizeX86Operation2])

  "an ClearInterruptFlag instruction" when {

    "in real mode" should {

      implicit val processorMode = ProcessorMode.Real

      "correctly encode cli" in {
        ClearInterruptFlag().encodeByte should be (Hex.lsb("FA"))
      }
    }

    "in long mode" should {

      implicit val processorMode = ProcessorMode.Long

      "correctly encode cli" in {
        ClearInterruptFlag().encodeByte should be (Hex.lsb("FA"))
      }
    }
  }
}