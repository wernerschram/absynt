package assembler.x86.instructions

import assembler.{Designation, Encodable, Hex}
import assembler.ListExtensions._
import assembler.sections.Section
import assembler.x86.ProcessorMode
import assembler.x86.operands.ImmediateValue._
import assembler.x86.operands.Register._
import assembler.x86.operands.memoryaccess._
import assembler.x86.operations.X86Operation
import org.scalatest.{Matchers, WordSpec}

class ArithmeticSuite extends WordSpec with Matchers {

  implicit val page: Section = Section(List.empty[Designation[Encodable]])

  // ADC, ADD, AND, CMP, OR, SBC, SUB and XOR all inherits from BasicInteraction.
  // BasicInteraction is covered by the XOR tests, for the others there are some testcases to test the opcode.

  "an AddCarry instruction" when {
    "in real mode" should {

      implicit val processorMode = ProcessorMode.Real

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

      implicit val processorMode = ProcessorMode.Real

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

      implicit val processorMode = ProcessorMode.Real

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

      implicit val processorMode = ProcessorMode.Real

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

      implicit val processorMode = ProcessorMode.Real

      "correctly encode not BYTE PTR [0x01]" in {
        Not(MemoryAddress.byteSize(0x0001.toShort.encodeLittleEndian)).encodeByte should be(Hex.lsb("F6 16 01 00"))
      }

      "correctly encode not WORD PTR [0x0001]" in {
        Not(MemoryAddress.wordSize(0x0001.toShort.encodeLittleEndian)).encodeByte should be(Hex.lsb("F7 16 01 00"))
      }
    }
    "in protected mode" should {

      implicit val processorMode = ProcessorMode.Protected

      "correctly encode not eax" in {
        Not(EAX).encodeByte should be(Hex.lsb("F7 D0"))
      }
    }

    "in long mode" should {

      implicit val processorMode = ProcessorMode.Long

      "correctly encode not QWORD PTR [rax]" in {
        Not(RegisterMemoryLocation.quadWordSize(RAX)).encodeByte should be(Hex.lsb("48 F7 10"))
      }
    }
  }

  "an Or instruction" when {
    "in real mode" should {

      implicit val processorMode = ProcessorMode.Real

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

      implicit val processorMode = ProcessorMode.Real

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

      implicit val processorMode = ProcessorMode.Real

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

      implicit val processorMode = ProcessorMode.Real

      "correctly encode xor al, 0x40" in {
        Xor(0x40.toByte, AL).encodeByte should be(Hex.lsb("34 40"))
      }

      "correctly encode xor ax, 0x3412" in {
        Xor(0x3412.toShort, AX).encodeByte should be(Hex.lsb("35 12 34"))
      }

      info("xor AL, AL can be encoded with -xor r/m8, r8- (30 C0) and with -xor r8, r/m8- (32 C0)")
      info("These are equal in every aspect. -xor r/m8, r8- is chosen to be the correct encoding")

      "correctly encode xor al, al" in {
        Xor(AL, AL).encodeByte should be(Hex.lsb("30 C0"))
      }

      info("xor AX, AX can be encoded with -xor r/m16, r16- (31 C0) and with -xor r16, r/m16- (33 C0)")
      info("These are equal in every aspect. -xor r/m16, r16- is chosen to be the correct encoding")

      "correctly encode xor ax, ax" in {
        Xor(AX, AX).encodeByte should be(Hex.lsb("31 C0"))
      }

      "correctly encode xor eax, 0x44332211" in {
        Xor(0x44332211, EAX).encodeByte should be(Hex.lsb("66 35 11 22 33 44"))
      }

      "correctly encode xor bl, 0x40" in {
        Xor(0x40.toByte, BL).encodeByte should be(Hex.lsb("80 F3 40"))
      }

      "correctly encode xor [bx], 0x3412" in {
        Xor(0x3412.toShort, RegisterMemoryLocation.wordSize(BX)).encodeByte should be(Hex.lsb("81 37 12 34"))
      }

      "correctly encode xor [bx], al" in {
        Xor(AL, RegisterMemoryLocation(BX)).encodeByte should be(Hex.lsb("30 07"))
      }

      "correctly encode xor ah, [si]" in {
        Xor(RegisterMemoryLocation(SI), AH).encodeByte should be(Hex.lsb("32 24"))
      }
    }

    "in protected mode" should {

      implicit val processorMode = ProcessorMode.Protected

      "correctly encode xor DWORD PTR [0x11111111], 0x44332211" in {
        Xor(0x44332211, MemoryAddress.doubleWordSize(0x11111111.encodeLittleEndian)).encodeByte should be(Hex.lsb("81 35 11 11 11 11 11 22 33 44"))
      }

      "correctly encode xor WORD PTR [0x11111111], 0x44" in {
        Xor(0x44.toByte, MemoryAddress.wordSize(0x11111111.encodeLittleEndian)).encodeByte should be(Hex.lsb("66 83 35 11 11 11 11 44"))
      }

      "correctly encode xor [0x1234], edx" in {
        Xor(EDX, MemoryAddress(0X1234.toShort.encodeLittleEndian)).encodeByte should be(Hex.lsb("67 31 16 34 12"))
      }

      "correctly encode xor esi, gs:[eax+0x12]" in {
        Xor(RegisterMemoryLocation.withSegmentOverride(EAX, 0x12.toByte.encodeLittleEndian, GS), ESI).encodeByte should be(Hex.lsb("65 33 70 12"))
      }
    }

    "in long mode" should {

      implicit val processorMode = ProcessorMode.Long

      "correctly encode xor QWORD PTR [0x11111111], 0x44332211" in {
        Xor(0x44332211, MemoryAddress.quadWordSize(0x11111111.encodeLittleEndian)).encodeByte should be(Hex.lsb("67 48 81 35 11 11 11 11 11 22 33 44"))
      }

      "throw an AssertionError for xor WORD PTR [0x11111111], 0x44332211" in {
        an[AssertionError] should be thrownBy {
          Xor(0x44332211, MemoryAddress.wordSize(0x11111111.encodeLittleEndian))
        }
      }

      "correctly encode xor rax, 0x78776655" in {
        Xor(0x78776655, RAX).encodeByte should be(Hex.lsb("48 35 55 66 77 78"))
      }

      // Note that the GNU assembler (and likely others) denotes this as xor RAX, 0xFFFFFFFF88776655 and doens't accept this notation.
      "correctly encode xor rax, 0x88776655" in {
        Xor(0x88776655, RAX).encodeByte should be(Hex.lsb("48 35 55 66 77 88"))
      }

      "throw an AssertionError for xor rax, 0x7877665544332211" in {
        an[AssertionError] should be thrownBy {
          Xor(0x7877665544332211l, RAX)
        }
      }

      "correctly encode xor BYTE PTR [rax+rbx*2], 0x11" in {
        Xor(0x11.toByte, SIBMemoryLocation.byteSize(RBX, RAX, scale = 2)).encodeByte should be(Hex.lsb("80 34 58 11"))
      }

      "correctly encode xor WORD PTR [rax+rbx*2], 0x2211" in {
        Xor(0x2211.toShort, SIBMemoryLocation.wordSize(RBX, RAX, scale = 2)).encodeByte should be(Hex.lsb("66 81 34 58 11 22"))
      }

      "correctly encode xor DWORD PTR [rax+rbx*2], 0x44332211" in {
        Xor(0x44332211, SIBMemoryLocation.doubleWordSize(RBX, RAX, scale = 2)).encodeByte should be(Hex.lsb("81 34 58 11 22 33 44"))
      }

      "correctly encode xor BYTE PTR gs:[rax+rbx*2], 0x11" in {
        Xor(0x11.toByte, SIBMemoryLocation.withSegmentOverride.byteSize(RBX, RAX, List.empty[Byte], 2, GS)).encodeByte should be(Hex.lsb("65 80 34 58 11"))
      }

      "correctly encode xor WORD PTR es:[rax+rbx*2], 0x2211" in {
        Xor(0x2211.toShort, SIBMemoryLocation.withSegmentOverride.wordSize(RBX, RAX, List.empty[Byte], 2, ES)).encodeByte should be(Hex.lsb("26 66 81 34 58 11 22"))
      }

      "correctly encode xor DWORD PTR fs:[rax+rbx*2], 0x44332211" in {
        Xor(0x44332211, SIBMemoryLocation.withSegmentOverride.doubleWordSize(RBX, RAX, List.empty[Byte], 2, FS)).encodeByte should be(Hex.lsb("64 81 34 58 11 22 33 44"))
      }

      "correctly encode xor QWORD PTR ss:[rax+rbx*2], 0x44332211" in {
        Xor(0x44332211, SIBMemoryLocation.withSegmentOverride.quadWordSize(RBX, RAX, List.empty[Byte], 2, SS)).encodeByte should be(Hex.lsb("36 48 81 34 58 11 22 33 44"))
      }

      "correctly encode xor QWORD PTR cs:[eax], 0x44332211" in {
        Xor(0x44332211, RegisterMemoryLocation.withSegmentOverride.quadWordSize(EAX, List.empty[Byte], CS)).encodeByte should be(Hex.lsb("2e 67 48 81 30 11 22 33 44"))
      }

      "correctly encode xor DWORD PTR cs:[rbx], 0x44332211" in {
        Xor(0x44332211, RegisterMemoryLocation.withSegmentOverride.doubleWordSize(RBX, List.empty[Byte], CS)).encodeByte should be(Hex.lsb("2e 81 33 11 22 33 44"))
      }

      "correctly encode xor WORD PTR cs:[rbx], 0x2211" in {
        Xor(0x2211.toShort, RegisterMemoryLocation.withSegmentOverride.wordSize(RBX, List.empty[Byte], CS)).encodeByte should be(Hex.lsb("2e 66 81 33 11 22"))
      }

      "correctly encode xor BYTE PTR cs:[rbx], 0x11" in {
        Xor(0x11.toByte, RegisterMemoryLocation.withSegmentOverride.byteSize(RBX, List.empty[Byte], CS)).encodeByte should be(Hex.lsb("2e 80 33 11"))
      }

      "correctly encode xor rbx, 0x44332211" in {
        Xor(0x44332211, RBX).encodeByte should be(Hex.lsb("48 81 F3 11 22 33 44"))
      }

      "correctly encode xor DWORD PTR [rax+rbx*2], 0x44" in {
        Xor(0x44.toByte, SIBMemoryLocation.doubleWordSize(RBX, RAX, scale = 2)).encodeByte should be(Hex.lsb("83 34 58 44"))
      }

      "correctly encode xor QWORD PTR [rax+rbx*2], 0x44" in {
        Xor(0x44.toByte, SIBMemoryLocation.quadWordSize(RBX, RAX, scale = 2)).encodeByte should be(Hex.lsb("48 83 34 58 44"))
      }

      "correctly encode xor [rbx], al" in {
        Xor(AL, RegisterMemoryLocation(RBX)).encodeByte should be(Hex.lsb("30 03"))
      }

      "correctly encode xor [rbx], ah" in {
        Xor(AH, RegisterMemoryLocation(RBX)).encodeByte should be(Hex.lsb("30 23"))
      }

      "correctly encode xor [rbx], r15b" in {
        Xor(R15L, RegisterMemoryLocation(RBX)).encodeByte should be(Hex.lsb("44 30 3B"))
      }

      "correctly encode xor [rax], rax" in {
        Xor(RAX, RegisterMemoryLocation(RAX)).encodeByte should be(Hex.lsb("48 31 00"))
      }

      "correctly encode xor r15, [r11]" in {
        Xor(RegisterMemoryLocation(R11), R15).encodeByte should be(Hex.lsb("4D 33 3B"))
      }
    }
  }
}
