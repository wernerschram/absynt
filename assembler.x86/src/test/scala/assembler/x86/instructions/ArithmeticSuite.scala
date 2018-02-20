package assembler.x86.instructions

import assembler.Hex
import assembler.x86.ProcessorMode
import assembler.x86.operands.ImmediateValue._
import assembler.x86.operands.Register._
import assembler.x86.operands.ValueSize
import assembler.x86.operands.memoryaccess._
import org.scalatest.{Matchers, WordSpec}

class ArithmeticSuite extends WordSpec with Matchers {

  // ADC, ADD, AND, CMP, OR, SBC, SUB and XOR all inherits from BasicInteraction.
  // BasicInteraction is covered by the XOR tests, for the others there are some testcases to test the opcode.

  "an AddCarry instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encodeByte adc al, 0x40" in {
        AddCarry(0x40.toByte, AL).encodeByte should be(Hex.lsb("14 40"))
      }

      "correctly encodeByte adc bl, 0x40" in {
        AddCarry(0x40.toByte, BL).encodeByte should be(Hex.lsb("80 D3 40"))
      }
    }
  }

  "an Add instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode add al, 0x40" in {
        Add(0x40.toByte, AL).encodeByte should be(Hex.lsb("04 40"))
      }

      "correctly encode add bl, 0x40" in {
        Add(0x40.toByte, BL).encodeByte should be(Hex.lsb("80 C3 40"))
      }
    }
  }

  "an And instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode and al, 0x40" in {
        And(0x40.toByte, AL).encodeByte should be(Hex.lsb("24 40"))
      }

      "correctly encode and bl, 0x40" in {
        And(0x40.toByte, BL).encodeByte should be(Hex.lsb("80 E3 40"))
      }
    }
  }

  "an Compare instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode cmp al, 0x40" in {
        Compare(0x40.toByte, AL).encodeByte should be(Hex.lsb("3C 40"))
      }

      "correctly encode cmp bl, 0x40" in {
        Compare(0x40.toByte, BL).encodeByte should be(Hex.lsb("80 FB 40"))
      }
    }
  }

  "an Not instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode not BYTE PTR [0x01]" in {
        Not(MemoryAddress.withSize(0x0001.toShort)(ValueSize.Byte)).encodeByte should be(Hex.lsb("F6 16 01 00"))
      }

      "correctly encode not WORD PTR [0x0001]" in {
        Not(MemoryAddress.withSize(0x0001.toShort)(ValueSize.Word)).encodeByte should be(Hex.lsb("F7 16 01 00"))
      }
    }
    "in protected mode" should {

      import ProcessorMode.Protected._

      "correctly encode not eax" in {
        Not(EAX).encodeByte should be(Hex.lsb("F7 D0"))
      }
    }

    "in long mode" should {

      import ProcessorMode.Long._

      "correctly encode not QWORD PTR [rax]" in {
        Not(RegisterMemoryLocation.withSize(RAX)(ValueSize.QuadWord)).encodeByte should be(Hex.lsb("48 F7 10"))
      }
    }
  }

  "an Or instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode or al, 0x40" in {
        Or(0x40.toByte, AL).encodeByte should be(Hex.lsb("0C 40"))
      }

      "correctly encode or bl, 0x40" in {
        Or(0x40.toByte, BL).encodeByte should be(Hex.lsb("80 CB 40"))
      }
    }
  }

  "an SubtractCarry instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode sbb al, 0x40" in {
        SubtractCarry(0x40.toByte, AL).encodeByte should be(Hex.lsb("1C 40"))
      }

      "correctly encode sbb bl, 0x40" in {
        SubtractCarry(0x40.toByte, BL).encodeByte should be(Hex.lsb("80 DB 40"))
      }
    }
  }

  "an Subtract instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode sub al, 0x40" in {
        Subtract(0x40.toByte, AL).encodeByte should be(Hex.lsb("2C 40"))
      }

      "correctly encode sub bl, 0x40" in {
        Subtract(0x40.toByte, BL).encodeByte should be(Hex.lsb("80 EB 40"))
      }
    }
  }

  "an Xor instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode xor al, 0x40" in {
        Xor(0x40.toByte, AL).encodeByte should be(Hex.lsb("34 40"))
      }

      "correctly represent xor al, 0x40 as a string" in {
        Xor(0x40.toByte, AL).toString shouldBe "xor al, 64"
      }

      "correctly encode xor ax, 0x3412" in {
        Xor(0x3412.toShort, AX).encodeByte should be(Hex.lsb("35 12 34"))
      }

      "correctly represent xor ax, 0x3412 as a string" in {
        Xor(0x3412.toShort, AX).toString shouldBe "xor ax, 13330"
      }

      info("xor AL, AL can be encoded with -xor r/m8, r8- (30 C0) and with -xor r8, r/m8- (32 C0)")
      info("These are equal in every aspect. -xor r/m8, r8- is chosen to be the correct encoding")

      "correctly encode xor al, al" in {
        Xor(AL, AL).encodeByte should be(Hex.lsb("30 C0"))
      }

      "correctly represent xor al, al as a string" in {
        Xor(AL, AL).toString shouldBe "xor al, al"
      }

      info("xor AX, AX can be encoded with -xor r/m16, r16- (31 C0) and with -xor r16, r/m16- (33 C0)")
      info("These are equal in every aspect. -xor r/m16, r16- is chosen to be the correct encoding")

      "correctly encode xor ax, ax" in {
        Xor(AX, AX).encodeByte should be(Hex.lsb("31 C0"))
      }

      "correctly represent xor ax, ax as a string" in {
        Xor(AX, AX).toString shouldBe "xor ax, ax"
      }

      "correctly encode xor eax, 0x44332211" in {
        Xor(0x44332211, EAX).encodeByte should be(Hex.lsb("66 35 11 22 33 44"))
      }

      "correctly represent xor ax, 0x44332211 as a string" in {
        Xor(0x44332211, EAX).toString shouldBe "xor eax, 1144201745"
      }

      "correctly encode xor bl, 0x40" in {
        Xor(0x40.toByte, BL).encodeByte should be(Hex.lsb("80 F3 40"))
      }

      "correctly represent xor bl, 0x40 as a string" in {
        Xor(0x40.toByte, BL).toString shouldBe "xor bl, 64"
      }

      "correctly encode xor WORD PTR [bx], 0x3412" in {
        Xor(0x3412.toShort, RegisterMemoryLocation.withSize(BX)(ValueSize.Word)).encodeByte should be(Hex.lsb("81 37 12 34"))
      }

      "correctly represent xor WORD PTR [bx], 0x3412 as a string" in {
        Xor(0x3412.toShort, RegisterMemoryLocation.withSize(BX)(ValueSize.Word)).toString shouldBe "xor WORD PTR [bx], 13330"
      }

      "correctly encode xor [bx], al" in {
        Xor(AL, RegisterMemoryLocation(BX)).encodeByte should be(Hex.lsb("30 07"))
      }

      "correctly represent xor [bx], al as a string" in {
        Xor(AL, RegisterMemoryLocation(BX)).toString shouldBe "xor [bx], al"
      }

      "correctly encode xor ah, [si]" in {
        Xor(RegisterMemoryLocation(SI), AH).encodeByte should be(Hex.lsb("32 24"))
      }

      "correctly represent xor ah, [si] as a string" in {
        Xor(RegisterMemoryLocation(SI), AH).toString shouldBe "xor ah, [si]"
      }

    }

    "in protected mode" should {

      import ProcessorMode.Protected._

      "correctly encode xor DWORD PTR [0x11111111], 0x44332211" in {
        Xor(0x44332211, MemoryAddress.withSize(0x11111111)(ValueSize.DoubleWord)).encodeByte should be(Hex.lsb("81 35 11 11 11 11 11 22 33 44"))
      }

      "correctly represent xor DWORD PTR [0x11111111], 0x44332211 as a string" in {
        Xor(0x44332211, MemoryAddress.withSize(0x11111111)(ValueSize.DoubleWord)).toString shouldBe "xor DWORD PTR [286331153], 1144201745"
      }

      "correctly encode xor WORD PTR [0x11111111], 0x44" in {
        Xor(0x44.toByte, MemoryAddress.withSize(0x11111111)(ValueSize.Word)).encodeByte should be(Hex.lsb("66 83 35 11 11 11 11 44"))
      }

      "correctly represent xor WORD PTR [0x11111111], 0x44 as a string" in {
        Xor(0x44.toByte, MemoryAddress.withSize(0x11111111)(ValueSize.Word)).toString shouldBe "xor WORD PTR [286331153], 68"
      }

      "correctly encode xor [0x1234], edx" in {
        Xor(EDX, MemoryAddress(0X1234.toShort)).encodeByte should be(Hex.lsb("67 31 16 34 12"))
      }

      "correctly represent xor [0x1234], edx as a string" in {
        Xor(EDX, MemoryAddress(0x1234.toShort)).toString shouldBe "xor [4660], edx"
      }

      "correctly encode xor esi, gs:[eax+0x12]" in {
        Xor(RegisterMemoryLocation.withSegmentOverride(EAX, 0x12.toByte, GS), ESI).encodeByte should be(Hex.lsb("65 33 70 12"))
      }

      "correctly represent xor esi, gs:[eax+0x12] as a string" in {
        Xor(RegisterMemoryLocation.withSegmentOverride(EAX, 0x12.toByte, GS), ESI).toString shouldBe "xor esi, gs:[eax+18]"
      }

    }

    "in long mode" should {

      import ProcessorMode.Long._

      "correctly encode xor QWORD PTR [0x11111111], 0x44332211" in {
        Xor(0x44332211, MemoryAddress.withSize(0x11111111)(ValueSize.QuadWord)).encodeByte should be(Hex.lsb("67 48 81 35 11 11 11 11 11 22 33 44"))
      }

      "correctly represent xor QWORD PTR [0x11111111], 0x44332211 as a string" in {
        Xor(0x44332211, MemoryAddress.withSize(0x11111111)(ValueSize.QuadWord)).toString shouldBe "xor QWORD PTR [286331153], 1144201745"
      }

      "throw an AssertionError for xor WORD PTR [0x11111111], 0x44332211" in {
        an[AssertionError] should be thrownBy {
          Xor(0x44332211, MemoryAddress.withSize(0x11111111)(ValueSize.Word))
        }
      }

      "correctly encode xor rax, 0x78776655" in {
        Xor(0x78776655, RAX).encodeByte should be(Hex.lsb("48 35 55 66 77 78"))
      }

      "correctly represent xor rax, 0x78776655 as a string" in {
        Xor(0x78776655, RAX).toString shouldBe "xor rax, 2021090901"
      }

      // Note that the GNU assembler (and likely others) denotes this as xor RAX, 0xFFFFFFFF88776655 and doens't accept this notation.
      "correctly encode xor rax, 0x88776655" in {
        Xor(0x88776655, RAX).encodeByte should be(Hex.lsb("48 35 55 66 77 88"))
      }

      "correctly represent xor rax, 0x88776655 as a string" in {
        Xor(0x88776655, RAX).toString shouldBe "xor rax, 2289526357"
      }

      "throw an AssertionError for xor rax, 0x7877665544332211" in {
        an[AssertionError] should be thrownBy {
          Xor(0x7877665544332211l, RAX)
        }
      }

      "correctly encode xor [rax+rbx*2], 0x11" in {
        Xor(0x11.toByte, SIBMemoryLocation(RBX, RAX, scale = 2)).encodeByte should be(Hex.lsb("80 34 58 11"))
      }

      "correctly represent xor [rax+rbx*2], 0x11 as a string" in {
        Xor(0x11.toByte, SIBMemoryLocation(RBX, RAX, scale = 2)).toString shouldBe "xor [rax+rbx*2], 17"
      }

      "correctly encode xor [rax+rbx*2], 0x2211" in {
        Xor(0x2211.toShort, SIBMemoryLocation(RBX, RAX, scale = 2)).encodeByte should be(Hex.lsb("66 81 34 58 11 22"))
      }

      "correctly represent xor [rax+rbx*2], 0x2211 as a string" in {
        Xor(0x2211.toShort, SIBMemoryLocation(RBX, RAX, scale = 2)).toString shouldBe "xor [rax+rbx*2], 8721"
      }

      "correctly encode xor [rax+rbx*2], 0x44332211" in {
        Xor(0x44332211, SIBMemoryLocation(RBX, RAX, scale = 2)).encodeByte should be(Hex.lsb("81 34 58 11 22 33 44"))
      }

      "correctly represent xor [rax+rbx*2], 0x44332211 as a string" in {
        Xor(0x44332211, SIBMemoryLocation(RBX, RAX, scale = 2)).toString shouldBe "xor [rax+rbx*2], 1144201745"
      }

      "correctly encode xor BYTE PTR [rax+rbx*2], 0x11" in {
        Xor(0x11.toByte, SIBMemoryLocation.byteSize(RBX, RAX, scale = 2)).encodeByte should be(Hex.lsb("80 34 58 11"))
      }

      "correctly represent xor BYTE PTR [rax+rbx*2], 0x11 as a string" in {
        Xor(0x11.toByte, SIBMemoryLocation.byteSize(RBX, RAX, scale = 2)).toString shouldBe "xor BYTE PTR [rax+rbx*2], 17"
      }

      "correctly encode xor WORD PTR [rax+rbx*2], 0x2211" in {
        Xor(0x2211.toShort, SIBMemoryLocation.wordSize(RBX, RAX, scale = 2)).encodeByte should be(Hex.lsb("66 81 34 58 11 22"))
      }

      "correctly represent xor WORD PTR [rax+rbx*2], 0x2211 as a string" in {
        Xor(0x2211.toShort, SIBMemoryLocation.wordSize(RBX, RAX, scale = 2)).toString shouldBe "xor WORD PTR [rax+rbx*2], 8721"
      }

      "correctly encode xor DWORD PTR [rax+rbx*2], 0x44332211" in {
        Xor(0x44332211, SIBMemoryLocation.doubleWordSize(RBX, RAX, scale = 2)).encodeByte should be(Hex.lsb("81 34 58 11 22 33 44"))
      }

      "correctly represent xor DWORD PTR [rax+rbx*2], 0x44332211 as a string" in {
        Xor(0x44332211, SIBMemoryLocation.doubleWordSize(RBX, RAX, scale = 2)).toString shouldBe "xor DWORD PTR [rax+rbx*2], 1144201745"
      }

      "correctly encode xor QWORD PTR [rax+rbx*2], 0x44332211" in {
        Xor(0x44332211, SIBMemoryLocation.quadWordSize(RBX, RAX, scale = 2)).encodeByte should be(Hex.lsb("48 81 34 58 11 22 33 44"))
      }

      "correctly represent xor QWORD PTR [rax+rbx*2], 0x44332211 as a string" in {
        Xor(0x44332211, SIBMemoryLocation.quadWordSize(RBX, RAX, scale = 2)).toString shouldBe "xor QWORD PTR [rax+rbx*2], 1144201745"
      }

      "correctly encode xor BYTE PTR gs:[rax+rbx*2], 0x11" in {
        Xor(0x11.toByte, SIBMemoryLocation.withSegmentOverride.byteSize(RBX, RAX, scale = 2, segment = GS)).encodeByte should be(Hex.lsb("65 80 34 58 11"))
      }

      "correctly represent xor BYTE PTR gs:[rax+rbx*2], 0x11 as a string" in {
        Xor(0x11.toByte, SIBMemoryLocation.withSegmentOverride.byteSize(RBX, RAX, scale = 2, segment = GS)).toString shouldBe "xor BYTE PTR gs:[rax+rbx*2], 17"
      }

      "correctly encode xor WORD PTR es:[rax+rbx*2], 0x2211" in {
        Xor(0x2211.toShort, SIBMemoryLocation.withSegmentOverride.wordSize(RBX, RAX, scale = 2, segment = ES)).encodeByte should be(Hex.lsb("26 66 81 34 58 11 22"))
      }

      "correctly represent xor WORD PTR es:[rax+rbx*2], 0x2211 as a string" in {
        Xor(0x2211.toShort, SIBMemoryLocation.withSegmentOverride.wordSize(RBX, RAX, scale = 2, segment = ES)).toString shouldBe "xor WORD PTR es:[rax+rbx*2], 8721"
      }

      "correctly encode xor DWORD PTR fs:[rax+rbx*2], 0x44332211" in {
        Xor(0x44332211, SIBMemoryLocation.withSegmentOverride.doubleWordSize(RBX, RAX, scale = 2, segment = FS)).encodeByte should be(Hex.lsb("64 81 34 58 11 22 33 44"))
      }

      "correctly represent xor DWORD PTR fs:[rax+rbx*2], 0x44332211 as a string" in {
        Xor(0x44332211, SIBMemoryLocation.withSegmentOverride.doubleWordSize(RBX, RAX, scale = 2, segment = FS)).toString shouldBe "xor DWORD PTR fs:[rax+rbx*2], 1144201745"
      }

      "correctly encode xor QWORD PTR ss:[rax+rbx*2], 0x44332211" in {
        Xor(0x44332211, SIBMemoryLocation.withSegmentOverride.quadWordSize(RBX, RAX, scale = 2, segment = SS)).encodeByte should be(Hex.lsb("36 48 81 34 58 11 22 33 44"))
      }

      "correctly represent xor QWORD PTR ss:[rax+rbx*2], 0x44332211 as a string" in {
        Xor(0x44332211, SIBMemoryLocation.withSegmentOverride.quadWordSize(RBX, RAX, scale = 2, segment = SS)).toString shouldBe "xor QWORD PTR ss:[rax+rbx*2], 1144201745"
      }

      "correctly encode xor QWORD PTR cs:[eax], 0x44332211" in {
        Xor(0x44332211, RegisterMemoryLocation.withSegmentOverride.withSize(EAX, segment = CS)(ValueSize.QuadWord)).encodeByte should be(Hex.lsb("2e 67 48 81 30 11 22 33 44"))
      }

      "correctly represent xor QWORD PTR cs:[eax], 0x44332211 as a string" in {
        Xor(0x44332211, RegisterMemoryLocation.withSegmentOverride.withSize(EAX, segment = CS)(ValueSize.QuadWord)).toString shouldBe "xor QWORD PTR cs:[eax], 1144201745"
      }

      "correctly encode xor DWORD PTR cs:[rbx], 0x44332211" in {
        Xor(0x44332211, RegisterMemoryLocation.withSegmentOverride.withSize(RBX, segment = CS)(ValueSize.DoubleWord)).encodeByte should be(Hex.lsb("2e 81 33 11 22 33 44"))
      }

      "correctly represent xor DWORD PTR cs:[rbx], 0x44332211 as a string" in {
        Xor(0x44332211, RegisterMemoryLocation.withSegmentOverride.withSize(RBX, segment = CS)(ValueSize.DoubleWord)).toString shouldBe "xor DWORD PTR cs:[rbx], 1144201745"
      }

      "correctly encode xor WORD PTR cs:[rbx], 0x2211" in {
        Xor(0x2211.toShort, RegisterMemoryLocation.withSegmentOverride.withSize(RBX, segment = CS)(ValueSize.Word)).encodeByte should be(Hex.lsb("2e 66 81 33 11 22"))
      }

      "correctly represent xor WORD PTR cs:[rbx], 0x2211 as a string" in {
        Xor(0x2211.toShort, RegisterMemoryLocation.withSegmentOverride.withSize(RBX, segment = CS)(ValueSize.Word)).toString shouldBe "xor WORD PTR cs:[rbx], 8721"
      }

      "correctly encode xor BYTE PTR cs:[rbx], 0x11" in {
        Xor(0x11.toByte, RegisterMemoryLocation.withSegmentOverride.withSize(RBX, segment = CS)(ValueSize.Byte)).encodeByte should be(Hex.lsb("2e 80 33 11"))
      }

      "correctly represent xor BYTE PTR cs:[rbx], 0x11 as a string" in {
        Xor(0x11.toByte, RegisterMemoryLocation.withSegmentOverride.withSize(RBX, segment = CS)(ValueSize.Byte)).toString shouldBe "xor BYTE PTR cs:[rbx], 17"
      }


      "correctly encode xor rbx, 0x44332211" in {
        Xor(0x44332211, RBX).encodeByte should be(Hex.lsb("48 81 F3 11 22 33 44"))
      }

      "correctly represent xor rbx, 0x44332211 as a string" in {
        Xor(0x44332211, RBX).toString shouldBe "xor rbx, 1144201745"
      }

      "correctly encode xor DWORD PTR [rax+rbx*2], 0x44" in {
        Xor(0x44.toByte, SIBMemoryLocation.doubleWordSize(RBX, RAX, scale = 2)).encodeByte should be(Hex.lsb("83 34 58 44"))
      }

      "correctly represent xor DWORD PTR [rax+rbx*2], 0x44 as a string" in {
        Xor(0x44.toByte, SIBMemoryLocation.doubleWordSize(RBX, RAX, scale = 2)).toString shouldBe "xor DWORD PTR [rax+rbx*2], 68"
      }

      "correctly encode xor QWORD PTR [rax+rbx*2], 0x44" in {
        Xor(0x44.toByte, SIBMemoryLocation.quadWordSize(RBX, RAX, scale = 2)).encodeByte should be(Hex.lsb("48 83 34 58 44"))
      }

      "correctly represent xor QWORD PTR [rax+rbx*2], 0x44 as a string" in {
        Xor(0x44.toByte, SIBMemoryLocation.quadWordSize(RBX, RAX, scale = 2)).toString shouldBe "xor QWORD PTR [rax+rbx*2], 68"
      }

      "correctly encode xor [rbx], al" in {
        Xor(AL, RegisterMemoryLocation(RBX)).encodeByte should be(Hex.lsb("30 03"))
      }

      "correctly represent xor [rbx], al as a string" in {
        Xor(AL, RegisterMemoryLocation(RBX)).toString shouldBe "xor [rbx], al"
      }

      "correctly encode xor [rbx], ah" in {
        Xor(AH, RegisterMemoryLocation(RBX)).encodeByte should be(Hex.lsb("30 23"))
      }

      "correctly represent xor [rbx], ah as a string" in {
        Xor(AH, RegisterMemoryLocation(RBX)).toString shouldBe "xor [rbx], ah"
      }

      "correctly encode xor [rbx], r15l" in {
        Xor(R15L, RegisterMemoryLocation(RBX)).encodeByte should be(Hex.lsb("44 30 3B"))
      }

      "correctly represent xor [rbx], r15l as a string" in {
        Xor(R15L, RegisterMemoryLocation(RBX)).toString shouldBe "xor [rbx], r15l"
      }

      "correctly encode xor [rax], rax" in {
        Xor(RAX, RegisterMemoryLocation(RAX)).encodeByte should be(Hex.lsb("48 31 00"))
      }

      "correctly represent xor [rax], rax as a string" in {
        Xor(RAX, RegisterMemoryLocation(RAX)).toString shouldBe "xor [rax], rax"
      }

      "correctly encode xor r15, [r11]" in {
        Xor(RegisterMemoryLocation(R11), R15).encodeByte should be(Hex.lsb("4D 33 3B"))
      }

      "correctly represent xor r15, [r11] as a string" in {
        Xor(RegisterMemoryLocation(R11), R15).toString shouldBe "xor r15, [r11]"
      }
    }
  }
}
