package assembler.x86.instructions.interrupt

import org.scalamock.scalatest.MockFactory
import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec

import assembler.Hex
import assembler.memory.MemoryPage
import assembler.x86.ProcessorMode
import assembler.x86.instructions.FixedSizeX86Instruction
import assembler.x86.instructions.Interrupt
import assembler.x86.operands.ImmediateValue._

class InterruptSuite extends WordSpec with ShouldMatchers with MockFactory {

  implicit val page: MemoryPage = new MemoryPage(List.empty[FixedSizeX86Instruction])

  "an Interrupt instruction" when {

    "in real mode" should {

      implicit val processorMode = ProcessorMode.Real

      "correctly encode int 0x03" in {
        Interrupt(0x03.toByte).encodeByte should be (Hex.LSB("CC"))
      }  
    }
    "in long mode" should {

      implicit val processorMode = ProcessorMode.Long

      "correctly encode int 0x01" in {
        Interrupt(0x01.toByte).encodeByte should be (Hex.LSB("CD 01"))
      }

      "throw an AssertionError for INT 0x0001" in {
        an [AssertionError] should be thrownBy {
          Interrupt(0x01.toShort) 
        }
      }
    } 
  }
}