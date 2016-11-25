package assembler.x86.instructions.arithmetic

import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec

import assembler.Hex
import assembler.memory.MemoryPage
import assembler.x86.ProcessorMode
import assembler.x86.operations.X86Operation
import assembler.x86.operands.ImmediateValue.byteToImmediate
import assembler.x86.operands.Register._

class SubtractSuite extends WordSpec with ShouldMatchers {

    implicit val page: MemoryPage = new MemoryPage(List.empty[X86Operation])

  // Subtract inherits from BasicInteraction, which is covered by the Xor instruction.
  // This suite covers two basic cases.

  "an Subtract instruction" when {
    "in real mode" should {

      implicit val processorMode = ProcessorMode.Real

      "correctly encode sub al, 0x40" in {
        Subtract(0x40.toByte, AL).encodeByte should be (Hex.lsb("2C 40"))
      }

      "correctly encode sub bl, 0x40" in {
        Subtract(0x40.toByte, BL).encodeByte should be (Hex.lsb("80 EB 40"))
      }
    }
  }
}