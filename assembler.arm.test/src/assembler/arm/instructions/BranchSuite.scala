package assembler.arm.instructions

import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec

import assembler.Hex
import assembler.MemoryPage
import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition
import assembler.arm.operands.RelativePointer.apply
import assembler.arm.operands.registers.GeneralRegister._

class BranchSuite extends WordSpec with ShouldMatchers {

  implicit val page: MemoryPage = new MemoryPage(List.empty[ARMInstruction])

  "an Branch instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode b PC+0x3e8" in {
        Branch(0x3e8).encodeByte should be(Hex.MSB("ea0000fa"))
      }

      "correctly encode beq PC+0x1111110" in {
        Branch(0x1111110, Condition.Equal).encodeByte should be(Hex.MSB("0a444444"))
      }

      "correctly encode blt PC-0x08" in {
        Branch(-0x08, Condition.SignedLessThan).encodeByte should be(Hex.MSB("bafffffe"))
      }
    }
  }

  "an BranchLink instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode bleq 0x1111118" in {
        BranchLink(0x1111110, Condition.Equal).encodeByte should be(Hex.MSB("0b444444"))
      }
    }
  }

  "an BranchLinkExchange instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode blx 0x123e" in {
        BranchLinkExchange(0x1234).encodeByte should be(Hex.MSB("fa00048d"))
      }

      "correctly encode blx r12" in {
        BranchLinkExchange(R12).encodeByte should be(Hex.MSB("e12fff3c"))
      }
    }
  }

  "a BranchExchange instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode bx r1" in {
        BranchExchange(R1).encodeByte should be(Hex.MSB("e12fff11"))
      }
    }
  }

  "a BranchExchangeJazelle instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode bxj r2" in {
        BranchExchangeJazelle(R2).encodeByte should be(Hex.MSB("e12fff22"))
      }
    }
  }
}