package assembler.x86.instructions

import assembler.Hex
import assembler.x86.ProcessorMode
import assembler.x86.operands.ImmediateValue._
import assembler.x86.operands.Register._
import assembler.x86.operands.ValueSize
import assembler.x86.operands.memoryaccess._
import org.scalatest.{Matchers, WordSpec}

class StackSuite extends WordSpec with Matchers {

  "an Push instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "throw an AssertionError for push BYTE PTR [bp]" in {
        an[AssertionError] should be thrownBy {
          Push(RegisterMemoryLocation.withSize(BP)(ValueSize.Byte)).encodeByte
        }
      }

      "correctly encode push WORD PTR [0x0001]" in {
        Push(MemoryAddress.withSize(0x0001.toShort)(ValueSize.Word)).encodeByte should be(Hex.lsb("FF 36 01 00"))
      }

      "correctly encode push DWORD PTR [bx+si]" in {
        Push(RegisterMemoryLocation.withSize(BX.combinedIndex(SI))(ValueSize.DoubleWord)).encodeByte should be(0x66.toByte :: 0xFF.toByte :: 0x30.toByte :: Nil)
      }

      "throw an AssertionError for push cl" in {
        an[AssertionError] should be thrownBy {
          Push(CL).encodeByte
        }
      }

      "correctly encode push bx" in {
        Push(BX).encodeByte should be(0x53.toByte :: Nil)
      }

      "correctly encode push 0x01" in {
        Push(0x01.toByte).encodeByte should be(0x6A.toByte :: 0x01.toByte :: Nil)
      }

      "correctly encode push 0x1980" in {
        Push(0x1980.toShort).encodeByte should be(0x68.toByte :: 0x80.toByte :: 0x19.toByte :: Nil)
      }

      "correctly encode push 0x00FF11EE" in {
        Push(0x00FF11EE).encodeByte should be(0x66.toByte :: 0x68.toByte :: 0xEE.toByte :: 0x11.toByte :: 0xFF.toByte :: 0x00.toByte :: Nil)
      }

      "correctly encode push cs" in {
        Push(CS).encodeByte should be(0x0E.toByte :: Nil)
      }
    }

    "in protected mode" should {

      import ProcessorMode.Protected._

      "correctly encode push eax" in {
        Push(EAX).encodeByte should be(0x50.toByte :: Nil)
      }

      "correctly encode push ss" in {
        Push(SS).encodeByte should be(0x16.toByte :: Nil)
      }

      "correctly encode push ds" in {
        Push(DS).encodeByte should be(0x1E.toByte :: Nil)
      }

      "correctly encode push es" in {
        Push(ES).encodeByte should be(0x06.toByte :: Nil)
      }

      "correctly encode push fs" in {
        Push(FS).encodeByte should be(0x0F.toByte :: 0xA0.toByte :: Nil)
      }

      "correctly encode push gs" in {
        Push(GS).encodeByte should be(0x0F.toByte :: 0xA8.toByte :: Nil)
      }

      "throw an AssertionError for push QWORD PTR [bp]" in {
        an[AssertionError] should be thrownBy {
          Push(RegisterMemoryLocation.withSize(BP)(ValueSize.QuadWord)).encodeByte
        }
      }
    }

    "in long mode" should {

      import ProcessorMode.Long._

      "correctly encode push QWORD PTR [rax]" in {
        Push(RegisterMemoryLocation.withSize(RAX)(ValueSize.QuadWord)).encodeByte should be(0x48.toByte :: 0xFF.toByte :: 0x30.toByte :: Nil)
      }

      "throw an AssertionError for push DWORD PTR [rax]" in {
        an[AssertionError] should be thrownBy {
          Push(RegisterMemoryLocation.withSize(RAX)(ValueSize.DoubleWord)).encodeByte
        }
      }

      "throw an AssertionError for push DWORD PTR [r13d]" in {
        an[AssertionError] should be thrownBy {
          Push(R13D).encodeByte
        }
      }

      "throw an AssertionError for push 0x1234567812345678" in {
        an[AssertionError] should be thrownBy {
          Push(0x1234567812345678l)
        }
      }

      "correctly encode push r13" in {
        Push(R13).encodeByte should be(0x41.toByte :: 0x55.toByte :: Nil)
      }

      "correctly encode push rbp" in {
        Push(RBP).encodeByte should be(0x55.toByte :: Nil)
      }

      "correctly encode push rsi" in {
        Push(RSI).encodeByte should be(0x56.toByte :: Nil)
      }
    }
  }

  "a PushAll instruction" when {

    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode pusha" in {
        PushAll().encodeByte should be(Hex.lsb("60"))
      }
    }

    "in long mode" should {

      import ProcessorMode.Long._

      "throw an AssertionError for pusha" in {
        an[AssertionError] should be thrownBy {
          PushAll().encodeByte
        }
      }
    }
  }

  "an PushFlags instruction" when {
    "in long mode" should {

      import ProcessorMode.Long._

      "correctly encode pushf" in {
        PushFlags().encodeByte should be(Hex.lsb("9C"))
      }
    }
  }
}