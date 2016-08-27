package assembler.x86.instructions.string

import org.scalamock.scalatest.MockFactory
import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec

import assembler.MemoryPage
import assembler.Hex
import assembler.x86.ProcessorMode
import assembler.x86.instructions.FixedSizeX86Instruction
import assembler.x86.operands.memoryaccess.RegisterMemoryLocation.indexWrapper
import assembler.x86.operands.registers.Register._

class StoreStringSuite extends WordSpec with ShouldMatchers with MockFactory {

  implicit val page: MemoryPage = new MemoryPage(List.empty[FixedSizeX86Instruction])

  "an StoreString instruction" when {
    "in real mode" should {

      implicit val processorMode = ProcessorMode.Real

      "correctly encode stos [di], al" in {
        StoreString(AL, DI).encode should be (Hex("AA"))
      }

      "correctly encode REP stos [di], al" in {
        StoreString.Repeat(AL, DI).encode should be (Hex("F3 AA"))
      }

      "correctly encode stos [di], ax" in {
        StoreString(AX, DI).encode should be (Hex("AB"))
      }

      "correctly encode stos [edi], ax" in {
        StoreString(AX, EDI).encode should be (Hex("67 AB"))
      }
    }
  
    "in protected mode" should {

      implicit val processorMode = ProcessorMode.Protected

      "correctly encode stos [edi], al" in {
        StoreString(AL, EDI).encode should be (Hex("AA"))
      }

      "correctly encode stos [di], al" in {
        StoreString(AL, DI).encode should be (Hex("67 AA"))
      }

      "correctly encode stos [di], ax" in {
        StoreString(AX, DI).encode should be (Hex("67 66 AB"))
      }

    }
  
    "in long mode" should {

      implicit val processorMode = ProcessorMode.Long
    }
  }
}