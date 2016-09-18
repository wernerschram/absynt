package assembler.x86.instructions.stack

import org.scalamock.scalatest.MockFactory
import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec

import assembler.Hex
import assembler.memory.MemoryPage
import assembler.x86.ProcessorMode
import assembler.x86.instructions.FixedSizeX86Instruction

class PushAllSuite extends WordSpec with ShouldMatchers with MockFactory {

  implicit val page: MemoryPage = new MemoryPage(List.empty[FixedSizeX86Instruction])

  "a PushAll instruction" when {

    "in real mode" should {

      implicit val processorMode = ProcessorMode.Real

      "correctly encode pusha" in {
        PushAll().encodeByte should be (Hex.lsb("60"))
      }
    }

    "in long mode" should {

      implicit val processorMode = ProcessorMode.Long

      "throw an AssertionError for pusha" in {
        an[AssertionError] should be thrownBy {
          PushAll()
        }
      }
    }
  }
}