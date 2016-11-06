package assembler.x86.instructions.arithmetic

import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec

import assembler.Hex
import assembler.ListExtensions.ShortEncoder
import assembler.memory.MemoryPage
import assembler.x86.ProcessorMode
import assembler.x86.instructions.FixedSizeX86Instruction
import assembler.x86.operands.memoryaccess.MemoryAddress
import assembler.x86.operands.memoryaccess.RegisterMemoryLocation
import assembler.x86.operands.Register._

class NotSuite extends WordSpec with ShouldMatchers {

  implicit val page: MemoryPage = new MemoryPage(List.empty[FixedSizeX86Instruction])

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
}