package assembler.x86.instructions.arithmetic

import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec

import assembler.Hex
import assembler.memory.MemoryPage
import assembler.x86.ProcessorMode
import assembler.x86.instructions.FixedSizeX86Operation2
import assembler.x86.operands.ImmediateValue.byteToImmediate
import assembler.x86.operands.Register.AL
import assembler.x86.operands.Register.BL

class AddSuite extends WordSpec with ShouldMatchers {

  implicit val page: MemoryPage = new MemoryPage(List.empty[FixedSizeX86Operation2])

  // Add inherits from BasicInteraction, which is covered by the Xor instruction.
  // This suite covers two basic cases.

  "an Add instruction" when {
    "in real mode" should {

      implicit val processorMode = ProcessorMode.Real

      "correctly encode add al, 0x40" in {
        Add(0x40.toByte, AL).encodeByte should be(Hex.lsb("04 40"))
      }

      "correctly encode add bl, 0x40" in {
        Add(0x40.toByte, BL).encodeByte should be(Hex.lsb("80 C3 40"))
      }
    }
  }
}