package assembler.x86

import org.scalamock.scalatest.MockFactory
import org.scalatest.ShouldMatchers
import org.scalatest.WordSpec

import assembler.Encodable
import assembler.Hex
import assembler.LabeledEncodable
import assembler.memory.MemoryPage
import assembler.x86.instructions.jump.Jump

class MemoryPageSuite extends WordSpec with ShouldMatchers with MockFactory {

  def filler(size: Int) = { 
    val filler = stub[Encodable]
    (filler.size()(_: MemoryPage)).when(*).returns(size)
    (filler.encodeByte()(_: MemoryPage)).when(*).returns(List.fill(size) { 0x00.toByte })
    filler
  }

  def labeledFiller(size: Int, label: String) = {
    val filler = stub[LabeledEncodable]
    (filler.size()(_: MemoryPage)).when(*).returns(size)
    (filler.label _).when.returns(label)
    (filler.encodeByte()(_: MemoryPage)).when(*).returns(List.fill(size) { 0x00.toByte })
    filler
  }

  "an MemoryPage" when {
    "in real mode" should {
      implicit val processorMode = ProcessorMode.Real

      "Encode a simple program with an indirect forward short jump instruction" in {
        val p = new MemoryPage(
          Jump("Label") ::
            filler(1) ::
            labeledFiller(1, "Label") ::
            Nil)

        p.encodeByte() should be(Hex.LSB("EB 01 00 00"))
      }

      "Encode a simple program with an indirect backward short jump instruction" in {
        val p = new MemoryPage(
          labeledFiller(1, "Label") ::
            filler(1) ::
            Jump("Label") ::
            Nil)

        p.encodeByte() should be(Hex.LSB("00 00 EB FC"))
      }

      "Encode a simple program with an indirect forward near jump instruction" in {
        val p = new MemoryPage(
          Jump("Label") ::
            filler(256) ::
            labeledFiller(1, "Label") ::
            Nil)

        p.encodeByte() should be(Hex.LSB("E9 00 01" + " 00" * 257))
      }

      "Encode a simple program with an indirect backward near jump instruction" in {
        val p = new MemoryPage(
          labeledFiller(1, "Label") ::
            filler(256) ::
            Jump("Label") ::
            Nil)

        p.encodeByte() should be(Hex.LSB("00 " * 257 + "E9 FC FE"))
      }

      "Encode a program with two indirect short jump instructions of which one jumps across the other" in {
        val p = new MemoryPage(
          labeledFiller(1, "Label1") ::
            Jump("Label2") ::
            filler(1) ::
            labeledFiller(1, "Label2") ::
            Jump("Label1") ::
            Nil)

        p.encodeByte() should be(Hex.LSB("00 EB 01 " + "00 " * 2 + "EB F9"))
      }

      "Encode a program with two indirect short jump instructions of which one jumps across the other and depends on the size of the other for its size" in {
        // Instruction 1 is the instruction that jumps to label 1
        // Instruction 2 is the instruction that jumps to label 2

        // The distance between instruction 1 and its labels is 124 + size of instruction 1 (short=2 or near=3) + size of instruction 2 (short=2 or near=3),
        // so the size of instruction 1 is short if instruction 2 is short (distance = 124 + 2 + 2 = 128)
        // and the size of instruction 1 is near if instruction 2 is near (distance = 123 + 3 + 3 = 130)

        val p = new MemoryPage(
          labeledFiller(1, "Label1") ::
            Jump("Label2") ::
            filler(122) ::
            labeledFiller(1, "Label2") ::
            Jump("Label1") ::
            Nil)

        p.encodeByte() should be(Hex.LSB("00 EB 7A " + "00 " * 123 + "EB 80"))
      }

      "Encode a program with two indirect short jump instructions that jump across eachother and depends on the size of the other for its size" in {
        // Instruction 1 is the instruction that jumps to label 1
        // Instruction 2 is the instruction that jumps to label 2

        // The distance between instruction 1 and its labels is 124 + size of instruction 1 (short=2 or near=3) + size of instruction 2 (short=2 or near=3),
        // so the size of instruction 1 is short if instruction 2 is short (distance = 124 + 2 + 2 = 128)
        // and the size of instruction 1 is near if instruction 2 is near (distance = 124 + 3 + 3 = 130)

        val p = new MemoryPage(
          labeledFiller(1, "Label1") ::
            Jump("Label2") ::
            filler(123) ::
            Jump("Label1") ::
            filler(2) ::
            labeledFiller(1, "Label2") ::
            Nil)

        p.encodeByte() should be(Hex.LSB("00 EB 7F " + "00 " * 123 + "EB 80 00 00 00"))
      }

      "Encode a program with two indirect near jump instructions that jump across eachother and depends on the size of the other for its size" in {
        // Instruction 1 is the instruction that jumps to label 1
        // Instruction 2 is the instruction that jumps to label 2

        // The distance between instruction 1 and its labels is 124 + size of instruction 1 (short=2 or near=3) + size of instruction 2 (short=2 or near=3),
        // so the size of instruction 1 is short if instruction 2 is short (distance = 124 + 2 + 2 = 128)
        // and the size of instruction 1 is near if instruction 2 is near (distance = 123 + 3 + 3 = 130)

        val p = new MemoryPage(
          labeledFiller(1, "Label1") ::
            Jump("Label2") ::
            filler(123) ::
            Jump("Label1") ::
            filler(3) ::
            labeledFiller(1, "Label2") ::
            Nil)

        p.encodeByte() should be(Hex.LSB("00 E9 80 00 " + "00 " * 123 + "E9 7E FF 00 00 00 00"))
      }

      "Encode a program with three indirect short jump instructions that jump across eachother and depends on the size of the others for its size" in {
        // Instruction 1 is the instruction that jumps to label 1
        // Instruction 2 is the instruction that jumps to label 2
        // Instruction 3 is the instruction that jumps to label 3

        // The distance between instruction 1 and its labels is 124 + size of instruction 1 + size of instruction 2 + size of instruction 3,
        // The size of instruction 1 is short if both the other instructions are short (distance = 123 + 2 + 2 = 127)
        // The size of instruction 2 is short if instruction 3 is short (distance = 125 + 2 = 127)
        // The size of instruction 3 is short if both the other instructions are short (distance = 124 + 2 + 2 = 128)
        // So, if one of them is near then all the others are near also

        val p = new MemoryPage(
          labeledFiller(1, "Label1") ::
            Jump("Label2") ::
            filler(60) ::
            Jump("Label3") ::
            filler(61) ::
            Jump("Label1") ::
            filler(2) ::
            labeledFiller(1, "Label2") ::
            filler(61) ::
            labeledFiller(1, "Label3") ::
            Nil)

        p.encodeByte() should be(Hex.LSB("00 EB 7F " + "00 " * 60 + "EB 7F " + "00 " * 61 + "EB 80" + " 00" * 65))
      }

      "Encode a program with three indirect near jump instructions that jump across eachother and depends on the size of the others for its size" in {
        // Instruction 1 is the instruction that jumps to label 1
        // Instruction 2 is the instruction that jumps to label 2
        // Instruction 3 is the instruction that jumps to label 3

        // The distance between instruction 1 and its labels is 124 + size of instruction 1 + size of instruction 2 + size of instruction 3,
        // The size of instruction 1 is short if both the other instructions are short (distance = 123 + 2 + 2 = 127)
        // The size of instruction 2 is short if instruction 3 is short (distance = 125 + 2 = 127)
        // The size of instruction 3 is short if both the other instructions are short (distance = 124 + 2 + 2 = 128)
        // So, if one of them is near then all the others are near also

        val p = new MemoryPage(
          labeledFiller(1, "Label1") ::
            Jump("Label2") ::
            filler(60) ::
            Jump("Label3") ::
            filler(61) ::
            Jump("Label1") ::
            filler(2) ::
            labeledFiller(1, "Label2") ::
            filler(62) ::
            labeledFiller(1, "Label3") ::
            Nil)

        p.encodeByte() should be(Hex.LSB("00 E9 80 00 " + "00 " * 60 + "E9 80 00 " + "00 " * 61 + "E9 7E FF" + " 00" * 66))
      }
    }

    "in protected mode" should {
      implicit val processorMode = ProcessorMode.Protected

      "Encode a simple program with an indirect backward short jump instruction" in {
        val p = new MemoryPage(
          labeledFiller(1, "Label") ::
            filler(1) ::
            Jump("Label") ::
            Nil)

        p.encodeByte() should be(Hex.LSB("00 00 EB FC"))
      }

      "Encode a simple program with an indirect forward near jump instruction" in {
        val p = new MemoryPage(
          Jump("Label") ::
            filler(256) ::
            labeledFiller(1, "Label") ::
            Nil)

        p.encodeByte() should be(Hex.LSB("E9 00 01 00 00" + " 00" * 257))
      }

    }
  }
}