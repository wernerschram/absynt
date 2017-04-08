package assembler.x86.instructions

import assembler.sections.Section
import assembler.x86.ProcessorMode
import assembler.x86.operands.ImmediateValue._
import assembler.x86.operands.Register._
import assembler.{Designation, Encodable, Hex, Label}
import org.scalatest.{Matchers, WordSpec}

class IOSuite extends WordSpec with Matchers {

  implicit val page: Section = Section(List.empty[Designation[Encodable]])

  "an Input instruction" when {
    "in real mode" should {

      implicit val processorMode = ProcessorMode.Real

      "correctly encode in al, 0x10" in {
        Input(0x10.toByte, AL).encodeByte should be(Hex.lsb("E4 10"))
      }

      "correctly represent in al, 0x10 as a string" in {
        Input(0x10.toByte, AL).toString should be("in al, 16")
      }

      "throw an Exception for in al, 0x0010" in {
        an[AssertionError] should be thrownBy {
          Input(0x0010.toShort, AL)
        }
      }

      "throw an Exception for in eax, 0x10" in {
        an[AssertionError] should be thrownBy {
          Input(0x10.toByte, EAX)
        }
      }

      "correctly encode in ax, 0x40" in {
        Input(0x20.toByte, AX).encodeByte should be(Hex.lsb("E5 20"))
      }

      "correctly represent in ax, 0x40 as a string" in {
        Input(0x40.toByte, AX).toString should be("in ax, 64")
      }

      "correctly encode in al, dx" in {
        Input(DX, AL).encodeByte should be(Hex.lsb("EC"))
      }

      "correctly represent in al, dx as a string" in {
        Input(DX, AL).toString should be("in al, dx")
      }

      "correctly encode in ax, dx" in {
        Input(DX, AX).encodeByte should be(Hex.lsb("ED"))
      }

      "correctly represent in ax, dx as a string" in {
        Input(DX, AX).toString should be("in ax, dx")
      }

      "correctly encode in eax, dx" in {
        Input(DX, EAX).encodeByte should be(Hex.lsb("66 ED"))
      }

      "correctly represent in eax, dx as a string" in {
        Input(DX, EAX).toString should be("in eax, dx")
      }
    }
    "in protected mode" should {
      implicit val processorMode = ProcessorMode.Protected

      "correctly encode in al, 0x10" in {
        Input(0x10.toByte, AL).encodeByte should be(Hex.lsb("E4 10"))
      }

      "correctly encode in ax, 0x40" in {
        Input(0x20.toByte, AX).encodeByte should be(Hex.lsb("E5 20"))
      }

      "correctly encode in al, dx" in {
        Input(DX, AL).encodeByte should be(Hex.lsb("EC"))
      }

      "correctly encode in ax, dx" in {
        Input(DX, AX).encodeByte should be(Hex.lsb("66 ED"))
      }

      "correctly encode in eax, dx" in {
        Input(DX, EAX).encodeByte should be(Hex.lsb("ED"))
      }

    }
    "in long mode" should {

      implicit val processorMode = ProcessorMode.Long

      "correctly encode in al, 0x10" in {
        Input(0x10.toByte, AL).encodeByte should be(Hex.lsb("E4 10"))
      }

      "correctly encode in ax, 0x40" in {
        Input(0x20.toByte, AX).encodeByte should be(Hex.lsb("E5 20"))
      }

      "correctly encode in al, dx" in {
        Input(DX, AL).encodeByte should be(Hex.lsb("EC"))
      }

      "correctly encode in ax, dx" in {
        Input(DX, AX).encodeByte should be(Hex.lsb("66 ED"))
      }

      "correctly encode in eax, dx" in {
        Input(DX, EAX).encodeByte should be(Hex.lsb("ED"))
      }

      "throw an Exception for in rax, dx" in {
        an[AssertionError] should be thrownBy {
          Input(DX, RAX)
        }
      }
    }
  }

  "an Output instruction" when {
    "in real mode" should {

      implicit val processorMode = ProcessorMode.Real

      "correctly encode out 0x10, al" in {
        Output(AL, 0x10.toByte).encodeByte should be(Hex.lsb("E6 10"))
      }

      "correctly represent out 0x10, al as a string" in {
        Output(AL, 0x10.toByte).toString should be("out 16, al")
      }

      "throw an Exception for out 0x0010, al" in {
        an[AssertionError] should be thrownBy {
          Output(AL, 0x0010.toShort)
        }
      }

      "throw an Exception for out 0x10, eax" in {
        an[AssertionError] should be thrownBy {
          Output(EAX, 0x10.toByte)
        }
      }

      "correctly encode out 0x20, ax" in {
        Output(AX, 0x20.toByte).encodeByte should be(Hex.lsb("E7 20"))
      }

      "correctly represent out 0x20, ax as a string" in {
        Output(AX, 0x20.toByte).toString should be("out 32, ax")
      }

      "correctly encode out dx, al" in {
        Output(AL, DX).encodeByte should be(Hex.lsb("EE"))
      }

      "correctly represent out dx, al as a string" in {
        Output(AL, DX).toString should be("out dx, al")
      }

      "correctly encode out dx, ax" in {
        Output(AX, DX).encodeByte should be(Hex.lsb("EF"))
      }

      "correctly represent out dx, ax as a string" in {
        Output(AX, DX).toString should be("out dx, ax")
      }

      "correctly encode out dx, eax" in {
        Output(EAX, DX).encodeByte should be(Hex.lsb("66 EF"))
      }

      "correctly represent out dx, eax as a string" in {
        Output(EAX, DX).toString should be("out dx, eax")
      }
    }
    "in protected mode" should {
      implicit val processorMode = ProcessorMode.Protected

      "correctly encode out 0x10, al" in {
        Output(AL, 0x10.toByte).encodeByte should be(Hex.lsb("E6 10"))
      }

      "correctly encode out 0x40, al" in {
        Output(AX, 0x20.toByte).encodeByte should be(Hex.lsb("E7 20"))
      }

      "correctly encode out dx, al" in {
        Output(AL, DX).encodeByte should be(Hex.lsb("EE"))
      }

      "correctly encode out dx, ax" in {
        Output(AX, DX).encodeByte should be(Hex.lsb("66 EF"))
      }

      "correctly encode out dx, eax" in {
        Output(EAX, DX).encodeByte should be(Hex.lsb("EF"))
      }

    }
    "in long mode" should {

      implicit val processorMode = ProcessorMode.Long

      "correctly encode out 0x10, al" in {
        Output(AL, 0x10.toByte).encodeByte should be(Hex.lsb("E6 10"))
      }

      "correctly encode out 0x20, ax" in {
        Output(AX, 0x20.toByte).encodeByte should be(Hex.lsb("E7 20"))
      }

      "correctly encode out dx, al" in {
        Output(AL, DX).encodeByte should be(Hex.lsb("EE"))
      }

      "correctly encode out dx, ax" in {
        Output(AX, DX).encodeByte should be(Hex.lsb("66 EF"))
      }

      "correctly encode out dx, eax" in {
        Output(EAX, DX).encodeByte should be(Hex.lsb("EF"))
      }

      "throw an Exception for out dx, rax" in {
        an[AssertionError] should be thrownBy {
          Output(RAX, DX)
        }
      }
    }
  }
}