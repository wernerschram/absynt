package assembler.sections

import assembler.{Encodable, EncodedByteList, Label, UniqueLabel}
import assembler.reference.{ReferencingInstruction, ReferencingInstructionOnPage}
import org.scalatest.{Matchers, WordSpec}

class SectionSuite extends WordSpec with Matchers {

  class MyReferencingInstructionOnPage(thisOperation: Encodable, label: Label)(implicit section: Section)
    extends ReferencingInstructionOnPage(thisOperation, label) {
    override def minimumSize: Int = 5

    override def maximumSize: Int = 5

    override def getSizeForDistance(forward: Boolean, distance: Int): Int = 5

    override def encodeForDistance(forward: Boolean, distance: Int)(implicit page: Section): List[Byte] = 0x01.toByte :: Nil
  }

  class MyReferencingInstruction(label: Label) extends ReferencingInstruction {
    override def getOrElseCreateInstruction()(implicit page: Section): ReferencingInstructionOnPage =
      new MyReferencingInstructionOnPage(this, label)

    override def size()(implicit page: Section): Int = 5
  }

  "a Section" should {
    "provide the intermediate instructions between a relative instruction and a label" in {
      val label = Label.unique
      val reference = new MyReferencingInstruction(label)
      val intermediate = EncodedByteList(List.fill(5)(0))
      val target = EncodedByteList(0.toByte :: Nil).withLabel(label)

      val section = Section(
        reference ::
          intermediate ::
          target ::
        Nil)

      section.intermediateEncodables(reference, label) should be(intermediate :: Nil)
    }

    "provide the intermediate instructions between a label and a relatie instruction" in {
      val label = Label.unique
      val reference = new MyReferencingInstruction(label)
      val intermediate = EncodedByteList(List.fill(5)(0))
      val target = EncodedByteList(0.toByte :: Nil).withLabel(label)

      val section = Section(
        target ::
          intermediate ::
          reference ::
          Nil)

      section.intermediateEncodables(reference, label) should be(target :: intermediate :: Nil)
    }
  }
}
