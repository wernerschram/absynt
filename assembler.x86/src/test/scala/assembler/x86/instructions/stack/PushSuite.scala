package assembler.x86.instructions.stack

import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec

import assembler.Hex
import assembler.ListExtensions.ShortEncoder
import assembler.memory.MemoryPage
import assembler.x86.ProcessorMode
import assembler.x86.instructions.FixedSizeX86Instruction
import assembler.x86.operands.ImmediateValue._
import assembler.x86.operands.memoryaccess._
import assembler.x86.operands.Register._

class PushSuite extends WordSpec with ShouldMatchers {

  implicit val page: MemoryPage = new MemoryPage(List.empty[FixedSizeX86Instruction])

  "an Push instruction" when {
    "in real mode" should {

      implicit val processorMode = ProcessorMode.Real

      "throw an AssertionError for push BYTE PTR [bp]" in {
        an [AssertionError] should be thrownBy {
          Push(RegisterMemoryLocation.byteSize(BP))
        }
      }

      "correctly encode push WORD PTR [0x0001]" in {
        Push(MemoryAddress.wordSize(0x0001.toShort.encodeLittleEndian)).encodeByte should be (Hex.lsb("FF 36 01 00"))
      }

      "correctly encode push DWORD PTR [bx+si]" in {
        Push(RegisterMemoryLocation.doubleWordSize(BX.combinedIndex(SI))).encodeByte should be (0x66.toByte :: 0xFF.toByte :: 0x30.toByte :: Nil)
      }

      "throw an AssertionError for push cl" in {
        an [AssertionError] should be thrownBy {
          Push(CL)
        }
      }

      "correctly encode push bx" in {
        Push(BX).encodeByte should be (0x53.toByte :: Nil)
      }

      "correctly encode push 0x01" in {
        Push(0x01.toByte).encodeByte should be (0x6A.toByte :: 0x01.toByte :: Nil)
      }

      "correctly encode push 0x1980" in {
        Push(0x1980.toShort).encodeByte should be (0x68.toByte :: 0x80.toByte :: 0x19.toByte :: Nil)
      }

      "correctly encode push 0x00FF11EE" in {
        Push(0x00FF11EE).encodeByte should be (0x66.toByte :: 0x68.toByte :: 0xEE.toByte :: 0x11.toByte :: 0xFF.toByte :: 0x00.toByte :: Nil)
      }

      "correctly encode push cs" in {
        Push(CS).encodeByte should be (0x0E.toByte :: Nil)
      }
    }

    "in protected mode" should {

      implicit val processorMode = ProcessorMode.Protected


      "correctly encode push eax" in {
        Push(EAX).encodeByte should be (0x50.toByte :: Nil)
      }

      "correctly encode push ss" in {
        Push(SS).encodeByte should be (0x16.toByte :: Nil)
      }

      "correctly encode push ds" in {
        Push(DS).encodeByte should be (0x1E.toByte :: Nil)
      }

      "correctly encode push es" in {
        Push(ES).encodeByte should be (0x06.toByte :: Nil)
      }

      "correctly encode push fs" in {
        Push(FS).encodeByte should be (0x0F.toByte :: 0xA0.toByte :: Nil)
      }

      "correctly encode push gs" in {
        Push(GS).encodeByte should be (0x0F.toByte :: 0xA8.toByte :: Nil)
      }
    }

    "in long mode" should {

      implicit val processorMode = ProcessorMode.Long

      "correctly encode push QWORD PTR [rax]" in {
        Push(RegisterMemoryLocation.quadWordSize(RAX)).encodeByte should be (0x48.toByte :: 0xFF.toByte :: 0x30.toByte :: Nil)
      }

      "throw an AssertionError for push DWORD PTR [rax]" in {
        an [AssertionError] should be thrownBy {
          Push(RegisterMemoryLocation.doubleWordSize(RAX))
        }
      }

      "throw an AssertionError for push 0x1234567812345678" in {
        an [AssertionError] should be thrownBy {
          Push(0x1234567812345678l)
        }
      }

      "correctly encode push r11" in {
        Push(R11).encodeByte should be (0x41.toByte :: 0x53.toByte :: Nil)
      }
    }
  }
}