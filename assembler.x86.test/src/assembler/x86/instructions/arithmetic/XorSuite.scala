package assembler.x86.instructions.arithmetic

import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec

import assembler.Hex
import assembler.ListExtensions._
import assembler.memory.MemoryPage
import assembler.x86.ProcessorMode
import assembler.x86.instructions.FixedSizeX86Instruction
import assembler.x86.operands.ImmediateValue._
import assembler.x86.operands.memoryaccess._
import assembler.x86.operands.registers.Register._

class XorSuite extends WordSpec with ShouldMatchers {

  implicit val page: MemoryPage = new MemoryPage(List.empty[FixedSizeX86Instruction])

  "an Xor instruction" when {
    "in real mode" should {

      implicit val processorMode = ProcessorMode.Real

      "correctly encode xor al, 0x40" in {
        Xor(0x40.toByte, AL).encodeByte should be(Hex.LSB("34 40"))
      }

      "correctly encode xor ax, 0x3412" in {
        Xor(0x3412.toShort, AX).encodeByte should be(Hex.LSB("35 12 34"))
      }

      info("xor AL, AL can be encoded with -xor r/m8, r8- (30 C0) and with -xor r8, r/m8- (32 C0)")
      info("These are equal in every aspect. -xor r/m8, r8- is chosen to be the correct encoding")
      
      "correctly encode xor al, al" in {
        Xor(AL, AL).encodeByte should be(Hex.LSB("30 C0"))
      }

      info("xor AX, AX can be encoded with -xor r/m16, r16- (31 C0) and with -xor r16, r/m16- (33 C0)")
      info("These are equal in every aspect. -xor r/m16, r16- is chosen to be the correct encoding")
      
      "correctly encode xor ax, ax" in {
        Xor(AX, AX).encodeByte should be(Hex.LSB("31 C0"))
      }

      "correctly encode xor eax, 0x44332211" in {
        Xor(0x44332211, EAX).encodeByte should be(Hex.LSB("66 35 11 22 33 44"))
      }

      "correctly encode xor bl, 0x40" in {
        Xor(0x40.toByte, BL).encodeByte should be(Hex.LSB("80 F3 40"))
      }

      "correctly encode xor [bx], 0x3412" in {
        Xor(0x3412.toShort, RegisterMemoryLocation.wordSize(BX)).encodeByte should be(Hex.LSB("81 37 12 34"))
      }

      "correctly encode xor [bx], al" in {
        Xor(AL, RegisterMemoryLocation(BX)).encodeByte should be(Hex.LSB("30 07"))
      }

      "correctly encode xor ah, [si]" in {
        Xor(RegisterMemoryLocation(SI), AH).encodeByte should be(Hex.LSB("32 24"))
      }
    }

    "in protected mode" should {

      implicit val processorMode = ProcessorMode.Protected

      "correctly encode xor DWORD PTR [0x11111111], 0x44332211" in {
        Xor(0x44332211, MemoryAddress.doubleWordSize(0x11111111.encodeLittleEndian)).encodeByte should be(Hex.LSB("81 35 11 11 11 11 11 22 33 44"))
      }

      "correctly encode xor WORD PTR [0x11111111], 0x44" in {
        Xor(0x44.toByte, MemoryAddress.wordSize(0x11111111.encodeLittleEndian)).encodeByte should be(Hex.LSB("66 83 35 11 11 11 11 44"))
      }

      "correctly encode xor [0x1234], edx" in {
        Xor(EDX, MemoryAddress(0X1234.toShort.encodeLittleEndian)).encodeByte should be(Hex.LSB("67 31 16 34 12"))
      }

      "correctly encode xor esi, gs:[eax+0x12]" in {
        Xor(RegisterMemoryLocation.withSegmentOverride(EAX, 0x12.toByte.encodeLittleEndian, GS), ESI).encodeByte should be(Hex.LSB("65 33 70 12"))
      }
    }

    "in long mode" should {

      implicit val processorMode = ProcessorMode.Long

      "correctly encode xor rax, 0x78776655" in {
        Xor(0x78776655, RAX).encodeByte should be(Hex.LSB("48 35 55 66 77 78"))
      }

      // Note that the GNU assembler (and likely others) denotes this as xor RAX, 0xFFFFFFFF88776655 and doens't accept this notation. 
      "correctly encode xor rax, 0x88776655" in {
        Xor(0x88776655, RAX).encodeByte should be(Hex.LSB("48 35 55 66 77 88"))
      }

      "throw an AssertionError for xor rax, 0x7877665544332211" in {
        an[AssertionError] should be thrownBy {
          Xor(0x7877665544332211l, RAX)
        }
      }

      "correctly encode xor DWORD PTR [rax+rbx*2], 0x44332211" in {
        Xor(0x44332211, SIBMemoryLocation.doubleWordSize(RBX, RAX, scale = 2)).encodeByte should be(Hex.LSB("81 34 58 11 22 33 44"))
      }

      "correctly encode xor rbx, 0x44332211" in {
        Xor(0x44332211, RBX).encodeByte should be(Hex.LSB("48 81 F3 11 22 33 44"))
      }

      "correctly encode xor DWORD PTR [rax+rbx*2], 0x44" in {
        Xor(0x44.toByte, SIBMemoryLocation.doubleWordSize(RBX, RAX, scale = 2)).encodeByte should be(Hex.LSB("83 34 58 44"))
      }

      "correctly encode xor [rbx], al" in {
        Xor(AL, RegisterMemoryLocation(RBX)).encodeByte should be(Hex.LSB("30 03"))
      }

      "correctly encode xor [rbx], ah" in {
        Xor(AH, RegisterMemoryLocation(RBX)).encodeByte should be(Hex.LSB("30 23"))
      }

      "correctly encode xor [rbx], r15b" in {
        Xor(R15L, RegisterMemoryLocation(RBX)).encodeByte should be(Hex.LSB("44 30 3B"))
      }

      "correctly encode xor [rax], rax" in {
        Xor(RAX, RegisterMemoryLocation(RAX)).encodeByte should be(Hex.LSB("48 31 00"))
      }

      "correctly encode xor r15, [r11]" in {
        Xor(RegisterMemoryLocation(R11), R15).encodeByte should be(Hex.LSB("4D 33 3B"))
      }
    }
  }
}