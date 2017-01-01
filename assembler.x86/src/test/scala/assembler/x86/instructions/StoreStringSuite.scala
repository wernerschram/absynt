package assembler.x86.instructions

import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec

import assembler.Hex
import assembler.memory.MemoryPage
import assembler.x86.ProcessorMode
import assembler.x86.operands.Register._
import assembler.x86.operands.memoryaccess.RegisterMemoryLocation
import assembler.x86.operands.memoryaccess.RegisterMemoryLocation.indexWrapper
import assembler.x86.operations.X86Operation

class StoreStringSuite extends WordSpec with ShouldMatchers {

  implicit val page: MemoryPage = new MemoryPage(List.empty[X86Operation])

  "an StoreString instruction" when {
    "in real mode" should {

      implicit val processorMode = ProcessorMode.Real

      "correctly encode stos [di], al" in {
        StoreString(AL, DI).encodeByte should be (Hex.lsb("AA"))
      }

      "correctly represent stos [di], al as a string" in {
        StoreString(AL, DI).toString should be("stos [di], al")
      }

      "correctly encode REP stos [di], al" in {
        StoreString.Repeat(AL, DI).encodeByte should be (Hex.lsb("F3 AA"))
      }

      "correctly represent rep stos [di], al as a string" in {
        StoreString.Repeat(AL, DI).toString should be("rep stos [di], al")
      }

      "correctly encode REP stos [di], ax" in {
        StoreString.Repeat(AX, DI).encodeByte should be (Hex.lsb("F3 AB"))
      }

      "correctly represent rep stos [di], ax as a string" in {
        StoreString.Repeat(AX, DI).toString should be("rep stos [di], ax")
      }

      "correctly encode stos [di], ax" in {
        StoreString(AX, DI).encodeByte should be (Hex.lsb("AB"))
      }

      "correctly represent stos [di], ax as a string" in {
        StoreString(AX, DI).toString should be("stos [di], ax")
      }

      "correctly encode stos [edi], ax" in {
        StoreString(AX, EDI).encodeByte should be (Hex.lsb("67 AB"))
      }

      "correctly represent rep stos [edi], ax as a string" in {
        StoreString.Repeat(AX, EDI).toString should be("rep stos [edi], ax")
      }

      "correctly encode stos cs:[edi], ax" in {
        StoreString(AX, EDI).encodeByte should be (Hex.lsb("67 AB"))
      }


      "correctly represent rep stos cs:[edi], ax as a string" in {
        StoreString.Repeat(AX, RegisterMemoryLocation(EDI, List.empty[Byte], CS)).toString should be("rep stos cs:[edi], ax")
      }
    }

    "in protected mode" should {

      implicit val processorMode = ProcessorMode.Protected

      "correctly encode stos [edi], al" in {
        StoreString(AL, EDI).encodeByte should be (Hex.lsb("AA"))
      }

      "correctly encode stos [di], al" in {
        StoreString(AL, DI).encodeByte should be (Hex.lsb("67 AA"))
      }

      "correctly encode stos [di], ax" in {
        StoreString(AX, DI).encodeByte should be (Hex.lsb("67 66 AB"))
      }

    }

    "in long mode" should {

      implicit val processorMode = ProcessorMode.Long
    }
  }
}