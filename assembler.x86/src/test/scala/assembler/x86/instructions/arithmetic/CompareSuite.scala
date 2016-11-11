package assembler.x86.instructions.arithmetic

import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec

import assembler.Hex
import assembler.memory.MemoryPage
import assembler.x86.ProcessorMode
import assembler.x86.instructions.FixedSizeX86Operation
import assembler.x86.operands.ImmediateValue.byteToImmediate
import assembler.x86.operands.Register._

class CompareSuite extends WordSpec with ShouldMatchers {

  implicit val page: MemoryPage = new MemoryPage(List.empty[FixedSizeX86Operation])

  // Add inherits from BasicInteraction, which is covered by the Xor instruction.
  // This suite covers two basic cases.

  "an Compare instruction" when {
    "in real mode" should {

      implicit val processorMode = ProcessorMode.Real

      "correctly encode cmp al, 0x40" in {
        Compare(0x40.toByte, AL).encodeByte should be (Hex.lsb("3C 40"))
      }

      "correctly encode cmp bl, 0x40" in {
        Compare(0x40.toByte, BL).encodeByte should be (Hex.lsb("80 FB 40"))
      }
    }
  }
}