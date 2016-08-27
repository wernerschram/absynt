package assembler.x86.instructions.arithmetic

import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec

import assembler.MemoryPage
import assembler.Hex
import assembler.x86.ProcessorMode
import assembler.x86.instructions.FixedSizeX86Instruction
import assembler.x86.operands.ImmediateValue.byteToImmediate
import assembler.x86.operands.registers.Register._

class AndSuite extends WordSpec with ShouldMatchers {

  implicit val page: MemoryPage = new MemoryPage(List.empty[FixedSizeX86Instruction])

  // And inherits from BasicInteraction, which is covered by the Xor instruction.
  // This suite covers two basic cases.
  
  "an And instruction" when {
    "in real mode" should {

      implicit val processorMode = ProcessorMode.Real
      
      "correctly encode and al, 0x40" in {
        And(0x40.toByte, AL).encode should be (Hex("24 40")) 
      }

      "correctly encode and bl, 0x40" in {
        And(0x40.toByte, BL).encode should be (Hex("80 E3 40"))
      }
    }
  }
}