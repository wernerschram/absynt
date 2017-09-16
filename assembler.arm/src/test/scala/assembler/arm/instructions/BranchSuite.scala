package assembler.arm.instructions

import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition
import assembler.arm.operands.registers.GeneralRegister._
import assembler.sections.Section
import assembler.{Resource, EncodedByteList, Hex, Label}
import org.scalatest.{Matchers, WordSpec}

class BranchSuite extends WordSpec with Matchers {

  implicit val page: Section = Section(List.empty[Resource], 0)

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
        val targetLabel = Label.unique
        val p = Section(List[Resource](
          Branch(targetLabel),
            EncodedByteList(List.fill(4)(0x00.toByte)),
            { implicit val label =  targetLabel; EncodedByteList(List.fill(4)(0x00.toByte))}), 0)

        p.encodable.encodeByte should be(Hex.msb("EA000000 00000000 00000000"))
      }

      "correctly encode a backward branch to a labeled instruction" in {
        val targetLabel: Label = "Label"
        val p = Section(List[Resource](
          { implicit val label =  targetLabel; EncodedByteList(List.fill(4)(0x00.toByte))},
            EncodedByteList(List.fill(4)(0x00.toByte)),
            Branch(targetLabel, Condition.LowerOrSame)), 0)

        p.encodable.encodeByte should be(Hex.msb("00000000 00000000 9AFFFFFC"))
      }

      "correctly encode a branch to self instruction" in {
        val targetLabel = Label.unique
        val p = Section(List[Resource](
          EncodedByteList(List.fill(8)(0x00.toByte)),
          { implicit val label =  targetLabel; Branch(targetLabel)},
          EncodedByteList(List.fill(8)(0x00.toByte))), 0)

        p.encodable.encodeByte should be(Hex.msb("00000000 00000000 EAFFFFFE 00000000 00000000"))
      }

      "correctly encode a forward branch over another branch to a labeled instruction" in {
        val targetLabel = Label.unique
        val p = Section(List[Resource](
          Branch(targetLabel),
            EncodedByteList(List.fill(4)(0x00.toByte)),
            Branch(targetLabel),
            EncodedByteList(List.fill(4)(0x00.toByte)),
          { implicit val label =  targetLabel; EncodedByteList(List.fill(4)(0x00.toByte))}), 0)

        p.encodable.encodeByte should be(Hex.msb("EA000002 00000000 EA000000 00000000 00000000"))
      }

      "correctly encode a backward branch over another branch to a labeled instruction" in {
        val targetLabel = Label.unique
        val p = Section(List[Resource](
          { implicit val label = targetLabel; EncodedByteList(List.fill(4)(0x00.toByte))},
            EncodedByteList(List.fill(4)(0x00.toByte)),
            Branch(targetLabel),
            EncodedByteList(List.fill(4)(0x00.toByte)),
            Branch(targetLabel)), 0)

        p.encodable.encodeByte should be(Hex.msb("00000000 00000000 EAFFFFFC 00000000 EAFFFFFA"))
      }

      "correctly represent b Label as a string" in {
        val targetLabel: Label = "Label"
        Branch(targetLabel).toString should be("b Label")
      }

      "correctly represent bne Label as a string" in {
        val targetLabel: Label = "Label"
        Branch(targetLabel, Condition.NotEqual).toString should be("bne Label")
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
        val targetLabel = Label.unique
        val p = Section(List[Resource](
          BranchLink(targetLabel),
            EncodedByteList(List.fill(4)(0x00.toByte)),
          { implicit val label =  targetLabel; EncodedByteList(List.fill(4)(0x00.toByte))}), 0)

        p.encodable.encodeByte should be(Hex.msb("EB000000 00000000 00000000"))
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
        val targetLabel = Label.unique
        val p = Section(List[Resource](
          BranchLinkExchange(targetLabel),
            EncodedByteList(List.fill(4)(0x00.toByte)),
          { implicit val label =  targetLabel; EncodedByteList(List.fill(4)(0x00.toByte))}), 0)

        p.encodable.encodeByte should be(Hex.msb("FA000000 00000000 00000000"))
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