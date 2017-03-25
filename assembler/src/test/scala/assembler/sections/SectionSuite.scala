package assembler.sections

import assembler._
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
      val target = EncodedByteList(0.toByte :: Nil)

      val section = Section(List[Designation[Encodable]](
        reference,
        intermediate,
        Labeled(label, target)))

      section.intermediateEncodables(reference, label) should be(intermediate :: Nil)
    }

    "provide the intermediate instructions between a label and a relative instruction" in {
      val label = Label.unique
      val reference = new MyReferencingInstruction(label)
      val intermediate = EncodedByteList(List.fill(5)(0))
      val target = EncodedByteList(0.toByte :: Nil)

      val section = Section(List[Designation[Encodable]](
        Labeled(label, target),
        intermediate,
        reference))

      section.intermediateEncodables(reference, label) should be(target :: intermediate :: Nil)
    }

    "know when a indirect reference is a forward reference" in {
      val label = Label.unique
      val reference = new MyReferencingInstruction(label)
      val target = EncodedByteList(0.toByte :: Nil)

      val section = Section(List[Designation[Encodable]](
        reference,
        Labeled(label, target)))

      section.isForwardReference(reference, label) should be(true)
    }

    "know when a indirect reference is a backward reference" in {
      val label = Label.unique
      val reference = new MyReferencingInstruction(label)
      val target = EncodedByteList(0.toByte :: Nil) //.withLabel(label)

      val section = Section(List[Designation[Encodable]](
        Labeled(label, target),
        reference))

      section.isForwardReference(reference, label) should be(false)
    }

    "be able to encode itself" in {
      val section = Section(List[Designation[Encodable]](
        EncodedByteList(0x00.toByte :: 0x01.toByte :: Nil),
        EncodedByteList(0xEF.toByte :: 0xFF.toByte :: Nil)))

      section.encodeByte should be(0x00.toByte :: 0x01.toByte :: 0xEF.toByte :: 0xFF.toByte :: Nil)
    }

    "correctly calculate its size" in {
      val oneSize = 4
      val twoSize = 6
      val one = EncodedByteList(List.fill(oneSize)(1))
      val two = EncodedByteList(List.fill(twoSize)(2))

      val section = Section(List[Designation[Encodable]](
        one,
        two))

      section.size should be(oneSize + twoSize)
    }

    "correctly provide the section relative address of a label " in {
      val label = Label.unique
      val intermediate = EncodedByteList(List.fill(5)(0))
      val target = EncodedByteList(0.toByte :: Nil) //.withLabel(label)

      val section = Section(List[Designation[Encodable]](
        intermediate,
          target))

      section.getRelativeAddress(target) should be(5)
    }
  }
}
