package assembler.x86.instructions.arithmetic

import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec

import assembler.Hex
import assembler.memory.MemoryPage
import assembler.x86.ProcessorMode
import assembler.x86.instructions.FixedSizeX86Instruction
import assembler.x86.operands.ImmediateValue.byteToImmediate
import assembler.x86.operands.registers.Register._

class AddCarrySuite extends WordSpec with ShouldMatchers {

  implicit val page: MemoryPage = new MemoryPage(List.empty[FixedSizeX86Instruction])

  // Add inherits from BasicInteraction, which is covered by the Xor instruction.
  // This suite covers two basic cases.

  "an AddCarry instruction" when {
    "in real mode" should {

      implicit val processorMode = ProcessorMode.Real

      "correctly encodeByte adc al, 0x40" in {
        AddCarry(0x40.toByte, AL).encodeByte should be (Hex.lsb("14 40"))
      }

      "correctly encodeByte adc bl, 0x40" in {
        AddCarry(0x40.toByte, BL).encodeByte should be (Hex.lsb("80 D3 40"))
      }
    }
  }
}