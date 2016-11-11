package assembler.x86.instructions.jump

import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec

import assembler.Hex
import assembler.ListExtensions._
import assembler.memory.MemoryPage
import assembler.x86.ProcessorMode
import assembler.x86.instructions.FixedSizeX86Operation
import assembler.x86.operands.memoryaccess._
import assembler.x86.operands.Register._

class JumpSuite extends WordSpec with ShouldMatchers {

  implicit val page: MemoryPage = new MemoryPage(List.empty[FixedSizeX86Operation])

  // NOTE: I used [next] to denote the location of the next instruction troughout this suite.
  // I can't find out an official way of denoting this. I keep myself recommended if someone knows an official notation :)

  "an Jump instruction" when {
    "in real mode" should {

      implicit val processorMode = ProcessorMode.Real

      "correctly encode jmp [next]+0x10" in { Jump(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("EB 10")) }
      "correctly encode ja [next]+0x10" in { JumpIfAbove(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("77 10")) }
      "correctly encode jae [next]+0x10" in { JumpIfAboveOrEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("73 10")) }
      "correctly encode jb [next]+0x10" in { JumpIfBelow(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("72 10")) }
      "correctly encode jbe [next]+0x10" in { JumpIfBelowOrEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("76 10")) }
      "correctly encode jc [next]+0x10" in { JumpIfCarry(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("72 10")) }
      "correctly encode je [next]+0x10" in { JumpIfEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("74 10")) }
      "correctly encode jg [next]+0x10" in { JumpIfGreater(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("7F 10")) }
      "correctly encode jge [next]+0x10" in { JumpIfGreaterOrEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("7D 10")) }
      "correctly encode jl [next]+0x10" in { JumpIfLess(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("7C 10")) }
      "correctly encode jle [next]+0x10" in { JumpIfLessOrEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("7E 10")) }
      "correctly encode jna [next]+0x10" in { JumpIfNotAbove(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("76 10")) }
      "correctly encode jnae [next]+0x10" in { JumpIfNotAboveOrEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("72 10")) }
      "correctly encode jnb [next]+0x10" in { JumpIfNotBelow(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("73 10")) }
      "correctly encode jnbe [next]+0x10" in { JumpIfNotBelowOrEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("77 10")) }
      "correctly encode jnc [next]+0x10" in { JumpIfNoCarry(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("73 10")) }
      "correctly encode jne [next]+0x10" in { JumpIfNotEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("75 10")) }
      "correctly encode jng [next]+0x10" in { JumpIfNotGreater(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("7E 10")) }
      "correctly encode jnge [next]+0x10" in { JumpIfNotGreaterOrEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("7C 10")) }
      "correctly encode jnl [next]+0x10" in { JumpIfNotLess(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("7D 10")) }
      "correctly encode jnle [next]+0x10" in { JumpIfNotLessOrEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("7F 10")) }

      "correctly encode jmp [next]+0x2030" in { Jump(NearPointer(0x2030.toShort.encodeLittleEndian)).encodeByte should be(Hex.lsb("E9 30 20")) }
      "correctly encode ja [next]+0x2030" in { JumpIfAbove(NearPointer(0x2030.toShort.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 87 30 20")) }
      "correctly encode jae [next]+0x2030" in { JumpIfAboveOrEqual(NearPointer(0x2030.toShort.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 83 30 20")) }
      "correctly encode jb [next]+0x2030" in { JumpIfBelow(NearPointer(0x2030.toShort.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 82 30 20")) }
      "correctly encode jbe [next]+0x2030" in { JumpIfBelowOrEqual(NearPointer(0x2030.toShort.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 86 30 20")) }
      "correctly encode jc [next]+0x2030" in { JumpIfCarry(NearPointer(0x2030.toShort.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 82 30 20")) }
      "correctly encode je [next]+0x2030" in { JumpIfEqual(NearPointer(0x2030.toShort.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 84 30 20")) }
      "correctly encode jg [next]+0x2030" in { JumpIfGreater(NearPointer(0x2030.toShort.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 8F 30 20")) }
      "correctly encode jge [next]+0x2030" in { JumpIfGreaterOrEqual(NearPointer(0x2030.toShort.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 8D 30 20")) }
      "correctly encode jl [next]+0x2030" in { JumpIfLess(NearPointer(0x2030.toShort.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 8C 30 20")) }
      "correctly encode jle [next]+0x2030" in { JumpIfLessOrEqual(NearPointer(0x2030.toShort.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 8E 30 20")) }
      "correctly encode jna [next]+0x2030" in { JumpIfNotAbove(NearPointer(0x2030.toShort.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 86 30 20")) }
      "correctly encode jnae [next]+0x2030" in { JumpIfNotAboveOrEqual(NearPointer(0x2030.toShort.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 82 30 20")) }
      "correctly encode jnb [next]+0x2030" in { JumpIfNotBelow(NearPointer(0x2030.toShort.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 83 30 20")) }
      "correctly encode jnbe [next]+0x2030" in { JumpIfNotBelowOrEqual(NearPointer(0x2030.toShort.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 87 30 20")) }
      "correctly encode jnc [next]+0x2030" in { JumpIfNoCarry(NearPointer(0x2030.toShort.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 83 30 20")) }
      "correctly encode jne [next]+0x2030" in { JumpIfNotEqual(NearPointer(0x2030.toShort.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 85 30 20")) }
      "correctly encode jng [next]+0x2030" in { JumpIfNotGreater(NearPointer(0x2030.toShort.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 8E 30 20")) }
      "correctly encode jnge [next]+0x2030" in { JumpIfNotGreaterOrEqual(NearPointer(0x2030.toShort.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 8C 30 20")) }
      "correctly encode jnl [next]+0x2030" in { JumpIfNotLess(NearPointer(0x2030.toShort.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 8D 30 20")) }
      "correctly encode jnle [next]+0x2030" in { JumpIfNotLessOrEqual(NearPointer(0x2030.toShort.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 8F 30 20")) }

      "correctly encode jcx [next]+0x10" in { JumpIfCountZero(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("E3 10")) }
      "throw an AssertionError for jcx [next]+0x2030" in { an[AssertionError] should be thrownBy { JumpIfCountZero(NearPointer(0x2030.encodeLittleEndian)) } }

      "throw an AssertionError for jmp [next]+0x10203040" in { an[AssertionError] should be thrownBy { Jump(NearPointer(0x10203040.encodeLittleEndian)) } }

      "correctly encode jmp ax" in {
        Jump(AX).encodeByte should be(Hex.lsb("FF E0"))
      }

      "correctly encode jmp [bp+si]" in {
        Jump(RegisterMemoryLocation(BP.combinedIndex(SI))).encodeByte should be(Hex.lsb("FF 22"))
      }

      "correctly encode jmp eax" in {
        Jump(EAX).encodeByte should be(Hex.lsb("66 FF E0"))
      }

      "correctly encode jmp DWORD PTR [eax]" in {
        Jump(RegisterMemoryLocation(EAX)).encodeByte should be(Hex.lsb("67 FF 20"))
      }

      "throw an AssertionError for jmp rax" in {
        an[AssertionError] should be thrownBy { Jump(RAX) }
      }

      "correctly encode jmp DWORD PTR fs:[bx+si]" in {
        Jump(RegisterMemoryLocation.withSegmentOverride.doubleWordSize(BX.combinedIndex(SI), segment = FS)).encodeByte should be(Hex.lsb("66 FF 20"))
      }

      "correctly encode jmp FAR 0x1000:0x2000" in {
        Jump.Far(new FarPointer(0x1000.toShort.encodeLittleEndian, 0x2000.toShort.encodeLittleEndian)).encodeByte should be(Hex.lsb("EA 00 20 00 10"))
      }

      "correctly encode jmp FAR 0x30:0x200010" in {
        Jump.Far(new FarPointer(0x30.toShort.encodeLittleEndian, 0x200010.encodeLittleEndian)).encodeByte should be(Hex.lsb("66 EA 10 00 20 00 30 00"))
      }

      "correctly encode jmp FAR WORD PTR [bp+si]" in {
        Jump.Far(RegisterMemoryLocation.segmentWordSize(BP.combinedIndex(SI))).encodeByte should be(Hex.lsb("FF 2A"))
      }

      "correctly encode jmp FAR DWORD PTR [bp+si]" in {
        Jump.Far(RegisterMemoryLocation.segmentDoubleWordSize(BP.combinedIndex(SI))).encodeByte should be(Hex.lsb("66 FF 2A"))
      }

      "throw an AssertionError for jmp FAR QWORD PTR [bp+si]" in {
        an[AssertionError] should be thrownBy {
          Jump.Far(RegisterMemoryLocation.segmentQuadWordSize(BP.combinedIndex(SI)))
        }
      }

    }

    "in protected mode" should {

      implicit val processorMode = ProcessorMode.Protected

      "correctly encode jmp [next]+0x10" in { Jump(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("EB 10")) }
      "correctly encode ja [next]+0x10" in { JumpIfAbove(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("77 10")) }
      "correctly encode jae [next]+0x10" in { JumpIfAboveOrEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("73 10")) }
      "correctly encode jb [next]+0x10" in { JumpIfBelow(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("72 10")) }
      "correctly encode jbe [next]+0x10" in { JumpIfBelowOrEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("76 10")) }
      "correctly encode jc [next]+0x10" in { JumpIfCarry(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("72 10")) }
      "correctly encode je [next]+0x10" in { JumpIfEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("74 10")) }
      "correctly encode jg [next]+0x10" in { JumpIfGreater(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("7F 10")) }
      "correctly encode jge [next]+0x10" in { JumpIfGreaterOrEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("7D 10")) }
      "correctly encode jl [next]+0x10" in { JumpIfLess(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("7C 10")) }
      "correctly encode jle [next]+0x10" in { JumpIfLessOrEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("7E 10")) }
      "correctly encode jna [next]+0x10" in { JumpIfNotAbove(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("76 10")) }
      "correctly encode jnae [next]+0x10" in { JumpIfNotAboveOrEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("72 10")) }
      "correctly encode jnb [next]+0x10" in { JumpIfNotBelow(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("73 10")) }
      "correctly encode jnbe [next]+0x10" in { JumpIfNotBelowOrEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("77 10")) }
      "correctly encode jnc [next]+0x10" in { JumpIfNoCarry(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("73 10")) }
      "correctly encode jne [next]+0x10" in { JumpIfNotEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("75 10")) }
      "correctly encode jng [next]+0x10" in { JumpIfNotGreater(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("7E 10")) }
      "correctly encode jnge [next]+0x10" in { JumpIfNotGreaterOrEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("7C 10")) }
      "correctly encode jnl [next]+0x10" in { JumpIfNotLess(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("7D 10")) }
      "correctly encode jnle [next]+0x10" in { JumpIfNotLessOrEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("7F 10")) }

      "throw an AssertionError for jmp [next]+0x1020" in {
        an[AssertionError] should be thrownBy {
          Jump(NearPointer(0x1020.toShort.encodeLittleEndian))
        }
      }

      "throw an AssertionError for ja [next]+0x1020" in {
        an[AssertionError] should be thrownBy {
          JumpIfAbove(NearPointer(0x1020.toShort.encodeLittleEndian))
        }
      }

      "correctly encode jmp [next]+0x20304050" in { Jump(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("E9 50 40 30 20")) }
      "correctly encode ja [next]+0x20304050" in { JumpIfAbove(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 87 50 40 30 20")) }
      "correctly encode jae [next]+0x20304050" in { JumpIfAboveOrEqual(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 83 50 40 30 20")) }
      "correctly encode jb [next]+0x20304050" in { JumpIfBelow(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 82 50 40 30 20")) }
      "correctly encode jbe [next]+0x20304050" in { JumpIfBelowOrEqual(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 86 50 40 30 20")) }
      "correctly encode jc [next]+0x20304050" in { JumpIfCarry(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 82 50 40 30 20")) }
      "correctly encode je [next]+0x20304050" in { JumpIfEqual(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 84 50 40 30 20")) }
      "correctly encode jg [next]+0x20304050" in { JumpIfGreater(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 8F 50 40 30 20")) }
      "correctly encode jge [next]+0x20304050" in { JumpIfGreaterOrEqual(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 8D 50 40 30 20")) }
      "correctly encode jl [next]+0x20304050" in { JumpIfLess(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 8C 50 40 30 20")) }
      "correctly encode jle [next]+0x20304050" in { JumpIfLessOrEqual(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 8E 50 40 30 20")) }
      "correctly encode jna [next]+0x20304050" in { JumpIfNotAbove(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 86 50 40 30 20")) }
      "correctly encode jnae [next]+0x20304050" in { JumpIfNotAboveOrEqual(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 82 50 40 30 20")) }
      "correctly encode jnb [next]+0x20304050" in { JumpIfNotBelow(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 83 50 40 30 20")) }
      "correctly encode jnbe [next]+0x20304050" in { JumpIfNotBelowOrEqual(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 87 50 40 30 20")) }
      "correctly encode jnc [next]+0x20304050" in { JumpIfNoCarry(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 83 50 40 30 20")) }
      "correctly encode jne [next]+0x20304050" in { JumpIfNotEqual(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 85 50 40 30 20")) }
      "correctly encode jng [next]+0x20304050" in { JumpIfNotGreater(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 8E 50 40 30 20")) }
      "correctly encode jnge [next]+0x20304050" in { JumpIfNotGreaterOrEqual(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 8C 50 40 30 20")) }
      "correctly encode jnl [next]+0x20304050" in { JumpIfNotLess(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 8D 50 40 30 20")) }
      "correctly encode jnle [next]+0x20304050" in { JumpIfNotLessOrEqual(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 8F 50 40 30 20")) }

      "correctly encode jmp si" in {
        Jump(AX).encodeByte should be(Hex.lsb("66 FF E0"))
      }

      "correctly encode jmp [bp+si]" in {
        Jump(RegisterMemoryLocation(BP.combinedIndex(SI))).encodeByte should be(Hex.lsb("67 FF 22"))
      }

      "correctly encode jmp eax" in {
        Jump(EAX).encodeByte should be(Hex.lsb("FF E0"))
      }

      "correctly encode jmp DWORD PTR [eax]" in {
        Jump(RegisterMemoryLocation(EAX)).encodeByte should be(Hex.lsb("FF 20"))
      }

      "throw an AssertionError for jmp rax" in {
        an[AssertionError] should be thrownBy {
          Jump(RAX)
        }
      }

      "correctly encode jmp DWORD PTR fs:[bx+si]" in {
        Jump(RegisterMemoryLocation.withSegmentOverride.doubleWordSize(BX.combinedIndex(SI), segment = FS)).encodeByte should be(Hex.lsb("67 FF 20"))
      }

      "correctly encode jmp FAR 0x1000:0x2000" in {
        Jump.Far(new FarPointer(0x1000.toShort.encodeLittleEndian, 0x2000.toShort.encodeLittleEndian)).encodeByte should be(Hex.lsb("66 EA 00 20 00 10"))
      }

      "correctly encode jmp FAR 0x30:0x200010" in {
        Jump.Far(new FarPointer(0x30.toShort.encodeLittleEndian, 0x200010.encodeLittleEndian)).encodeByte should be(Hex.lsb("EA 10 00 20 00 30 00"))
      }

      "correctly encode jmp FAR WORD PTR [bp+si]" in {
        Jump.Far(RegisterMemoryLocation.segmentWordSize(BP.combinedIndex(SI))).encodeByte should be(Hex.lsb("66 67 FF 2A"))
      }

      "correctly encode jmp FAR DWORD PTR [bp+si]" in {
        Jump.Far(RegisterMemoryLocation.segmentDoubleWordSize(BP.combinedIndex(SI))).encodeByte should be(Hex.lsb("67 FF 2A"))
      }

      "throw an AssertionError for jmp FAR QWORD PTR [bp+si]" in {
        an[AssertionError] should be thrownBy {
          Jump.Far(RegisterMemoryLocation.segmentQuadWordSize(BP.combinedIndex(SI)))
        }
      }

    }

    "in long mode" should {

      implicit val processorMode = ProcessorMode.Long

      "correctly encode jmp [next]+0x10" in { Jump(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("EB 10")) }
      "correctly encode ja [next]+0x10" in { JumpIfAbove(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("77 10")) }
      "correctly encode jae [next]+0x10" in { JumpIfAboveOrEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("73 10")) }
      "correctly encode jb [next]+0x10" in { JumpIfBelow(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("72 10")) }
      "correctly encode jbe [next]+0x10" in { JumpIfBelowOrEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("76 10")) }
      "correctly encode jc [next]+0x10" in { JumpIfCarry(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("72 10")) }
      "correctly encode je [next]+0x10" in { JumpIfEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("74 10")) }
      "correctly encode jg [next]+0x10" in { JumpIfGreater(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("7F 10")) }
      "correctly encode jge [next]+0x10" in { JumpIfGreaterOrEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("7D 10")) }
      "correctly encode jl [next]+0x10" in { JumpIfLess(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("7C 10")) }
      "correctly encode jle [next]+0x10" in { JumpIfLessOrEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("7E 10")) }
      "correctly encode jna [next]+0x10" in { JumpIfNotAbove(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("76 10")) }
      "correctly encode jnae [next]+0x10" in { JumpIfNotAboveOrEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("72 10")) }
      "correctly encode jnb [next]+0x10" in { JumpIfNotBelow(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("73 10")) }
      "correctly encode jnbe [next]+0x10" in { JumpIfNotBelowOrEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("77 10")) }
      "correctly encode jnc [next]+0x10" in { JumpIfNoCarry(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("73 10")) }
      "correctly encode jne [next]+0x10" in { JumpIfNotEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("75 10")) }
      "correctly encode jng [next]+0x10" in { JumpIfNotGreater(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("7E 10")) }
      "correctly encode jnge [next]+0x10" in { JumpIfNotGreaterOrEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("7C 10")) }
      "correctly encode jnl [next]+0x10" in { JumpIfNotLess(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("7D 10")) }
      "correctly encode jnle [next]+0x10" in { JumpIfNotLessOrEqual(NearPointer(0x10.toByte.encodeLittleEndian)).encodeByte should be(Hex.lsb("7F 10")) }

      "throw an AssertionError for jmp [next]+0x1020" in {
        an[AssertionError] should be thrownBy {
          Jump(NearPointer(0x1020.toShort.encodeLittleEndian))
        }
      }

      "throw an AssertionError for ja [next]+0x1020" in {
        an[AssertionError] should be thrownBy {
          JumpIfAbove(NearPointer(0x1020.toShort.encodeLittleEndian))
        }
      }

      "correctly encode jmp [next]+0x20304050" in { Jump(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("E9 50 40 30 20")) }
      "correctly encode ja [next]+0x20304050" in { JumpIfAbove(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 87 50 40 30 20")) }
      "correctly encode jae [next]+0x20304050" in { JumpIfAboveOrEqual(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 83 50 40 30 20")) }
      "correctly encode jb [next]+0x20304050" in { JumpIfBelow(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 82 50 40 30 20")) }
      "correctly encode jbe [next]+0x20304050" in { JumpIfBelowOrEqual(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 86 50 40 30 20")) }
      "correctly encode jc [next]+0x20304050" in { JumpIfCarry(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 82 50 40 30 20")) }
      "correctly encode je [next]+0x20304050" in { JumpIfEqual(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 84 50 40 30 20")) }
      "correctly encode jg [next]+0x20304050" in { JumpIfGreater(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 8F 50 40 30 20")) }
      "correctly encode jge [next]+0x20304050" in { JumpIfGreaterOrEqual(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 8D 50 40 30 20")) }
      "correctly encode jl [next]+0x20304050" in { JumpIfLess(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 8C 50 40 30 20")) }
      "correctly encode jle [next]+0x20304050" in { JumpIfLessOrEqual(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 8E 50 40 30 20")) }
      "correctly encode jna [next]+0x20304050" in { JumpIfNotAbove(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 86 50 40 30 20")) }
      "correctly encode jnae [next]+0x20304050" in { JumpIfNotAboveOrEqual(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 82 50 40 30 20")) }
      "correctly encode jnb [next]+0x20304050" in { JumpIfNotBelow(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 83 50 40 30 20")) }
      "correctly encode jnbe [next]+0x20304050" in { JumpIfNotBelowOrEqual(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 87 50 40 30 20")) }
      "correctly encode jnc [next]+0x20304050" in { JumpIfNoCarry(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 83 50 40 30 20")) }
      "correctly encode jne [next]+0x20304050" in { JumpIfNotEqual(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 85 50 40 30 20")) }
      "correctly encode jng [next]+0x20304050" in { JumpIfNotGreater(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 8E 50 40 30 20")) }
      "correctly encode jnge [next]+0x20304050" in { JumpIfNotGreaterOrEqual(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 8C 50 40 30 20")) }
      "correctly encode jnl [next]+0x20304050" in { JumpIfNotLess(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 8D 50 40 30 20")) }
      "correctly encode jnle [next]+0x20304050" in { JumpIfNotLessOrEqual(NearPointer(0x20304050.encodeLittleEndian)).encodeByte should be(Hex.lsb("0F 8F 50 40 30 20")) }

      "throw an AssertionError for jmp ax" in {
        an[AssertionError] should be thrownBy {
          Jump(AX)
        }
      }

      "throw an AssertionError for jmp [bp+si]" in {
        an[AssertionError] should be thrownBy {
          Jump(RegisterMemoryLocation(BP.combinedIndex(SI)))
        }
      }

      "throw an AssertionError for jmp eax" in {
        an[AssertionError] should be thrownBy {
          Jump(EAX)
        }
      }

      "correctly encode jmp [eax]" in {
        Jump(RegisterMemoryLocation(EAX)).encodeByte should be(Hex.lsb("67 FF 20"))
      }

      "correctly encode jmp rax" in {
        Jump(RAX).encodeByte should be(Hex.lsb("FF E0"))
      }

      "correctly encode jmp FAR 0x1000:0x2000" in {
        Jump.Far(new FarPointer(0x1000.toShort.encodeLittleEndian, 0x2000.toShort.encodeLittleEndian)).encodeByte should be(Hex.lsb("66 EA 00 20 00 10"))
      }

      "correctly encode jmp FAR 0x30:0x200010" in {
        Jump.Far(new FarPointer(0x30.toShort.encodeLittleEndian, 0x200010.encodeLittleEndian)).encodeByte should be(Hex.lsb("EA 10 00 20 00 30 00"))
      }

      "correctly encode jmp FAR WORD PTR [edx]" in {
        Jump.Far(RegisterMemoryLocation.segmentWordSize(EDX)).encodeByte should be(Hex.lsb("66 67 FF 2A"))
      }

      "correctly encode jmp FAR DWORD PTR [edx]" in {
        Jump.Far(RegisterMemoryLocation.segmentDoubleWordSize(EDX)).encodeByte should be(Hex.lsb("67 FF 2A"))
      }

      "correctly encode jmp FAR QWORD PTR [rdx]" in {
        Jump.Far(RegisterMemoryLocation.segmentQuadWordSize(RDX)).encodeByte should be(Hex.lsb("48 FF 2A"))
      }
    }
  }
}