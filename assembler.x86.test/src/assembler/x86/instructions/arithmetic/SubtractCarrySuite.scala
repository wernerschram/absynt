package assembler.x86.instructions.arithmetic

import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec

import assembler.Hex
import assembler.memory.MemoryPage
import assembler.x86.ProcessorMode
import assembler.x86.instructions.FixedSizeX86Instruction
import assembler.x86.operands.ImmediateValue.byteToImmediate
import assembler.x86.operands.registers.Register._

class SubtractCarrySuite extends WordSpec with ShouldMatchers {

    implicit val page: MemoryPage = new MemoryPage(List.empty[FixedSizeX86Instruction])

  // SubtractCarry inherits from BasicInteraction, which is covered by the Xor instruction.
  // This suite covers two basic cases.

  "an SubtractCarry instruction" when {
    "in real mode" should {

      implicit val processorMode = ProcessorMode.Real
      
      "correctly encode sbb al, 0x40" in {
        SubtractCarry(0x40.toByte, AL).encodeByte should be (Hex.LSB("1C 40")) 
      }
  
      "correctly encode sbb bl, 0x40" in {
        SubtractCarry(0x40.toByte, BL).encodeByte should be (Hex.LSB("80 DB 40"))
      }
    }
  }
}