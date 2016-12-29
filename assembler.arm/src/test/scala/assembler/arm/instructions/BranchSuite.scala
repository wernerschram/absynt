package assembler.arm.instructions

import org.scalamock.scalatest.MockFactory
import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec

import assembler.Condition.apply
import assembler.Encodable
import assembler.Hex
import assembler.LabeledEncodable
import assembler.StringLabel
import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition
import assembler.arm.operands.registers.GeneralRegister._
import assembler.memory.MemoryPage
import assembler.arm.operations.ARMOperation
import assembler.Label

class BranchSuite extends WordSpec with ShouldMatchers with MockFactory {

  def filler(size: Int) = {
    val filler = stub[Encodable]
    (filler.size()(_: MemoryPage)).when(*).returns(size)
    (filler.encodeByte()(_: MemoryPage)).when(*).returns(List.fill(size) { 0x00.toByte })
    filler
  }

  def labeledFiller(size: Int, label: Label) = {
    val filler = stub[LabeledEncodable]
    (filler.size()(_: MemoryPage)).when(*).returns(size)
    (filler.label _).when.returns(label)
    (filler.encodeByte()(_: MemoryPage)).when(*).returns(List.fill(size) { 0x00.toByte })
    filler
  }

  implicit val page: MemoryPage = new MemoryPage(List.empty[ARMOperation])

  "an Branch instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode b +0x3e8" in {
        Branch(0x3e8).encodeByte should be(Hex.msb("ea0000fa"))
      }

      "correctly encode beq +0x1111110" in {
        Branch(0x1111110, Condition.Equal).encodeByte should be(Hex.msb("0a444444"))
      }

      "correctly encode blt -0x08" in {
        Branch(-0x08, Condition.SignedLessThan).encodeByte should be(Hex.msb("bafffffe"))
      }

      "correctly represent blt -0x08 as a string" in {
        Branch(-0x08, Condition.SignedLessThan).toString should be("blt -8")
      }

      "correctly encode a forward branch to a labeled instruction" in {
        val label = Label.unique
        val p = new MemoryPage(
          Branch(label) ::
            filler(4) ::
            labeledFiller(4, label) ::
            Nil)

        p.encodeByte() should be(Hex.msb("EA000000 00000000 00000000"))
      }

      "correctly encode a backward branch to a labeled instruction" in {
        val label: Label = "Label"
        val p = new MemoryPage(
          labeledFiller(4, label) ::
            filler(4) ::
            Branch(label, Condition.LowerOrSame) ::
            Nil)

        p.encodeByte() should be(Hex.msb("00000000 00000000 9AFFFFFC"))
      }

      "correctly encode a forward branch over another branch to a labeled instruction" in {
        val label = Label.unique
        val p = new MemoryPage(
          Branch(label) ::
            filler(4) ::
            Branch(label) ::
            filler(4) ::
            labeledFiller(4, label) ::
            Nil)

        p.encodeByte() should be(Hex.msb("EA000002 00000000 EA000000 00000000 00000000"))
      }

      "correctly encode a backward branch over another branch to a labeled instruction" in {
        val label = Label.unique
        val p = new MemoryPage(
          labeledFiller(4, label) ::
            filler(4) ::
            Branch(label) ::
            filler(4) ::
            Branch(label) ::
            Nil)

        p.encodeByte() should be(Hex.msb("00000000 00000000 EAFFFFFC 00000000 EAFFFFFA"))
      }

      "correctly represent b Label as a string" in {
        val label: Label = "Label"
        Branch(label).toString should be("b Label")
      }
    }
  }

  "an BranchLink instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode bleq 0x1111118" in {
        BranchLink(0x1111110, Condition.Equal).encodeByte should be(Hex.msb("0b444444"))
      }

      "correctly represent bleq 0x1111118 as a string" in {
        BranchLink(0x1111110, Condition.Equal).toString should be("bleq 17895696")
      }

      "correctly encode a forward branch-link to a labeled instruction" in {
        val label = Label.unique
        val p = new MemoryPage(
          BranchLink(label) ::
            filler(4) ::
            labeledFiller(4, label) ::
            Nil)

        p.encodeByte() should be(Hex.msb("EB000000 00000000 00000000"))
      }
    }
  }

  "an BranchLinkExchange instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode blx 0x123c" in {
        BranchLinkExchange(0x1234).encodeByte should be(Hex.msb("fa00048d"))
      }

      "correctly represent blx 0x123c as a string" in {
        BranchLinkExchange(0x1234).toString should be("blx 4660")
      }

      "correctly encode blx 0x123d" in {
        BranchLinkExchange(0x1235).encodeByte should be(Hex.msb("fb00048d"))
      }

      "correctly represent blx 0x123d as a string" in {
        BranchLinkExchange(0x1235).toString should be("blx 4661")
      }

      "correctly encode blx r12" in {
        BranchLinkExchange(R12).encodeByte should be(Hex.msb("e12fff3c"))
      }


      "correctly encode a forward branch-link-exchange to a labeled instruction" in {
        val label = Label.unique
        val p = new MemoryPage(
          BranchLinkExchange(label) ::
            filler(4) ::
            labeledFiller(4, label) ::
            Nil)

        p.encodeByte() should be(Hex.msb("FA000000 00000000 00000000"))
      }
    }
  }

  "a BranchExchange instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode bx r1" in {
        BranchExchange(R1).encodeByte should be(Hex.msb("e12fff11"))
      }

      "correctly represent bx r1 as a string" in {
        BranchExchange(R1).toString should be("bx r1")
      }

    }
  }

  "a BranchExchangeJazelle instruction" when {
    "in a32 mode" should {

      implicit val processorMode = ProcessorMode.A32

      "correctly encode bxj r2" in {
        BranchExchangeJazelle(R2).encodeByte should be(Hex.msb("e12fff22"))
      }


      "correctly represent bxj r2 as a string" in {
        BranchExchangeJazelle(R2).toString should be("bxj r2")
      }
    }
  }
}