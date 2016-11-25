package assembler.x86.instructions.io

import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec

import assembler.Hex
import assembler.memory.MemoryPage
import assembler.x86.ProcessorMode
import assembler.x86.operations.X86Operation
import assembler.x86.operands.ImmediateValue._
import assembler.x86.operands.Register._

class InputSuite extends WordSpec with ShouldMatchers {

  implicit val page: MemoryPage = new MemoryPage(List.empty[X86Operation])

  "an Input instruction" when {
    "in real mode" should {

      implicit val processorMode = ProcessorMode.Real

      "correctly encode in al, 0x10" in {
        Input(0x10.toByte, AL).encodeByte should be (Hex.lsb("E4 10"))
      }

      "throw an Exception for in al, 0x0010" in {
        an [AssertionError] should be thrownBy {
          Input(0x0010.toShort, AL)
        }
      }

      "correctly encode in ax, 0x40" in {
        Input(0x20.toByte, AX).encodeByte should be (Hex.lsb("E5 20"))
      }

      "correctly encode in al, dx" in {
        Input(DX, AL).encodeByte should be (Hex.lsb("EC"))
      }

      "correctly encode in ax, dx" in {
        Input(DX, AX).encodeByte should be (Hex.lsb("ED"))
      }

      "correctly encode in eax, dx" in {
        Input(DX, EAX).encodeByte should be (Hex.lsb("66 ED"))
      }
    }
    "in protected mode" should {
      implicit val processorMode = ProcessorMode.Protected

      "correctly encode in al, 0x10" in {
        Input(0x10.toByte, AL).encodeByte should be (Hex.lsb("E4 10"))
      }

      "correctly encode in ax, 0x40" in {
        Input(0x20.toByte, AX).encodeByte should be (Hex.lsb("E5 20"))
      }

      "correctly encode in al, dx" in {
        Input(DX, AL).encodeByte should be (Hex.lsb("EC"))
      }

      "correctly encode in ax, dx" in {
        Input(DX, AX).encodeByte should be (Hex.lsb("66 ED"))
      }

      "correctly encode in eax, dx" in {
        Input(DX, EAX).encodeByte should be (Hex.lsb("ED"))
      }

    }
    "in long mode" should {

      implicit val processorMode = ProcessorMode.Long

      "correctly encode in al, 0x10" in {
        Input(0x10.toByte, AL).encodeByte should be (Hex.lsb("E4 10"))
      }

      "correctly encode in ax, 0x40" in {
        Input(0x20.toByte, AX).encodeByte should be (Hex.lsb("E5 20"))
      }

      "correctly encode in al, dx" in {
        Input(DX, AL).encodeByte should be (Hex.lsb("EC"))
      }

      "correctly encode in ax, dx" in {
        Input(DX, AX).encodeByte should be (Hex.lsb("66 ED"))
      }

      "correctly encode in eax, dx" in {
        Input(DX, EAX).encodeByte should be (Hex.lsb("ED"))
      }

      "throw an Exception for in rax, dx" in {
        an [AssertionError] should be thrownBy {
          Input(DX, RAX)
        }
      }
    }
  }
}