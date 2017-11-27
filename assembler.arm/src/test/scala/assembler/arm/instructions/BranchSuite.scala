package assembler.arm.instructions

import assembler.arm.ProcessorMode
import assembler.arm.operands._
import assembler.arm.operands.registers.GeneralRegister._
import assembler.sections.{Section, SectionType}
import assembler._
import assembler.output.raw.Raw
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}

class BranchSuite extends WordSpec with Matchers with MockFactory {

  "an Branch instruction" when {
    "in a32 mode" should {

      import ProcessorMode.A32._

      "correctly encode b +0x3e8" in {
        Branch(RelativeA32Pointer(ArmRelativeOffset(0x3e8))).encodeByte should be(Hex.msb("ea0000fa"))
      }

      "correctly encode beq +0x1111110" in {
        Branch(RelativeA32Pointer(ArmRelativeOffset(0x1111110)), Condition.Equal).encodeByte should be(Hex.msb("0a444444"))
      }

      "correctly encode blt -0x08" in {
        Branch(RelativeA32Pointer(ArmRelativeOffset(-0x08)), Condition.SignedLessThan).encodeByte should be(Hex.msb("bafffffe"))
      }

      "correctly represent blt -0x08 as a string" in {
        Branch(RelativeA32Pointer(ArmRelativeOffset(-0x08)), Condition.SignedLessThan).toString should be("blt 0x00000000")
      }

      "correctly encode a forward branch to a labeled instruction" in {
        val targetLabel = Label.unique
        val p = Section(SectionType.Text, ".test", List[Resource](
          Branch(targetLabel),
            EncodedByteList(List.fill(4)(0x00.toByte)),
            { implicit val label: UniqueLabel =  targetLabel; EncodedByteList(List.fill(4)(0x00.toByte))}))

        val application: Application[ArmOffset] = Raw[ArmOffset](p, 0)
        application.encodableSections.head.encodeByte should be(Hex.msb("EA000000 00000000 00000000"))
      }

      "correctly encode a backward branch to a labeled instruction" in {
        val targetLabel: Label = "Label"
        val p = Section(SectionType.Text, ".test", List[Resource](
          { implicit val label: Label =  targetLabel; EncodedByteList(List.fill(4)(0x00.toByte))},
            EncodedByteList(List.fill(4)(0x00.toByte)),
            Branch(targetLabel, Condition.LowerOrSame)))

        val application: Application[ArmOffset] = Raw[ArmOffset](p, 0)
        application.encodableSections.head.encodeByte should be(Hex.msb("00000000 00000000 9AFFFFFC"))
      }

      "correctly encode a branch to self instruction" in {
        val targetLabel = Label.unique
        val p = Section(SectionType.Text, ".test", List[Resource](
          EncodedByteList(List.fill(8)(0x00.toByte)),
          { implicit val label: UniqueLabel =  targetLabel; Branch(targetLabel)},
          EncodedByteList(List.fill(8)(0x00.toByte))))

        val application: Application[ArmOffset] = Raw[ArmOffset](p, 0)
        application.encodableSections.head.encodeByte should be(Hex.msb("00000000 00000000 EAFFFFFE 00000000 00000000"))
      }

      "correctly encode a forward branch over another branch to a labeled instruction" in {
        val targetLabel = Label.unique
        val p = Section(SectionType.Text, ".test", List[Resource](
          Branch(targetLabel),
            EncodedByteList(List.fill(4)(0x00.toByte)),
            Branch(targetLabel),
            EncodedByteList(List.fill(4)(0x00.toByte)),
          { implicit val label: UniqueLabel =  targetLabel; EncodedByteList(List.fill(4)(0x00.toByte))}))

        val application: Application[ArmOffset] = Raw[ArmOffset](p, 0)
        application.encodableSections.head.encodeByte should be(Hex.msb("EA000002 00000000 EA000000 00000000 00000000"))
      }

      "correctly encode a backward branch over another branch to a labeled instruction" in {
        val targetLabel = Label.unique
        val p = Section(SectionType.Text, ".test", List[Resource](
          { implicit val label: UniqueLabel = targetLabel; EncodedByteList(List.fill(4)(0x00.toByte))},
            EncodedByteList(List.fill(4)(0x00.toByte)),
            Branch(targetLabel),
            EncodedByteList(List.fill(4)(0x00.toByte)),
            Branch(targetLabel)))

        val application: Application[ArmOffset] = Raw[ArmOffset](p, 0)
        application.encodableSections.head.encodeByte should be(Hex.msb("00000000 00000000 EAFFFFFC 00000000 EAFFFFFA"))
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

      import ProcessorMode.A32._

      "correctly encode bleq 0x1111118" in {
        BranchLink(RelativeA32Pointer(ArmRelativeOffset(0x1111110)), Condition.Equal).encodeByte should be(Hex.msb("0b444444"))
      }

      "correctly represent bleq 0x01111118 as a string" in {
        BranchLink(RelativeA32Pointer(ArmRelativeOffset(0x1111110)), Condition.Equal).toString should be("bleq 0x01111118")
      }

      "correctly encode a forward branch-link to a labeled instruction" in {
        val targetLabel = Label.unique
        val p = Section(SectionType.Text, ".test", List[Resource](
          BranchLink(targetLabel),
            EncodedByteList(List.fill(4)(0x00.toByte)),
          { implicit val label: UniqueLabel =  targetLabel; EncodedByteList(List.fill(4)(0x00.toByte))}))

        val application: Application[ArmOffset] = Raw[ArmOffset](p, 0)
        application.encodableSections.head.encodeByte should be(Hex.msb("EB000000 00000000 00000000"))
      }
    }
  }

  "an BranchLinkExchange instruction" when {
    "in a32 mode" should {

      import ProcessorMode.A32._

      "correctly encode blx 0x123C" in {
        BranchLinkExchange(RelativeThumbPointer(ArmRelativeOffset(0x1234))).encodeByte should be(Hex.msb("fa00048d"))
      }

      "correctly represent blx 0x123C as a string" in {
        BranchLinkExchange(RelativeThumbPointer(ArmRelativeOffset(0x1234))).toString should be("blx 0x0000123C")
      }

      "correctly encode blx 0x123E" in {
        BranchLinkExchange(RelativeThumbPointer(ArmRelativeOffset(0x1236))).encodeByte should be(Hex.msb("fb00048d"))
      }

      "correctly represent blx 0x123E as a string" in {
        BranchLinkExchange(RelativeThumbPointer(ArmRelativeOffset(0x1236))).toString should be("blx 0x0000123E")
      }

      "correctly encode blx r12" in {
        BranchLinkExchange(R12).encodeByte should be(Hex.msb("e12fff3c"))
      }

      "correctly encode a forward branch-link-exchange to a labeled instruction" in {
        val targetLabel = Label.unique
        val p = Section(SectionType.Text, ".test", List[Resource](
          BranchLinkExchange(targetLabel),
            EncodedByteList(List.fill(4)(0x00.toByte)),
          { implicit val label: UniqueLabel =  targetLabel; EncodedByteList(List.fill(4)(0x00.toByte))}))

        val application: Application[ArmOffset] = Raw[ArmOffset](p, 0)
        application.encodableSections.head.encodeByte should be(Hex.msb("FA000000 00000000 00000000"))
      }
    }
  }

  "a BranchExchange instruction" when {
    "in a32 mode" should {

      import ProcessorMode.A32._

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

      import ProcessorMode.A32._

      "correctly encode bxj r2" in {
        BranchExchangeJazelle(R2).encodeByte should be(Hex.msb("e12fff22"))
      }

      "correctly represent bxj r2 as a string" in {
        BranchExchangeJazelle(R2).toString should be("bxj r2")
      }
    }
  }
}