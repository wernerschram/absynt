package assembler.sections

import assembler._
import assembler.reference.RelativeReference
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}

class SectionSuite extends WordSpec with Matchers with MockFactory {

  val application: Application = mock[Application]

  "a Section" when {
    "queried for immediate instructions" should {

      "provide the intermediate instructions between a relative instruction and a label" in {
        val label = Label.unique
        val myRelativeReference: RelativeReference = mock[RelativeReference]
        (myRelativeReference.target _).expects().returning(label).anyNumberOfTimes()
        (myRelativeReference.label _).expects().returning(NoLabel()).anyNumberOfTimes()
        val reference = myRelativeReference
        val intermediate = EncodedByteList(List.fill(5)(0))
        val target = EncodedByteList(0.toByte :: Nil)(label)

        val section = Section(List[Resource](
          reference,
          intermediate,
          target), 0)

        section.intermediateEncodables(reference) should be(intermediate :: Nil)
      }

      "provide the intermediate instructions between a label and a relative instruction" in {
        val label = Label.unique
        val reference: RelativeReference = mock[RelativeReference]
        (reference.target _).expects().returning(label).anyNumberOfTimes()
        (reference.label _).expects().returning(NoLabel()).anyNumberOfTimes()
        val intermediate = EncodedByteList(List.fill(5)(0))
        val target = EncodedByteList(0.toByte :: Nil)(label)

        val section = Section(List[Resource](
          target,
          intermediate,
          reference), 0)

        section.intermediateEncodables(reference) should be(target :: intermediate :: Nil)
      }

      "return an empty list for an instruction that references itself" in {
        val targetLabel = Label.unique
        val reference: RelativeReference = mock[RelativeReference]
        (reference.target _).expects().returning(targetLabel).anyNumberOfTimes()
        (reference.label _).expects().returning(targetLabel).anyNumberOfTimes()
        val prefix = EncodedByteList(List.fill(2)(0))
        val postfix = EncodedByteList(List.fill(3)(0))

        val section = Section(List[Resource](
          prefix,
          reference,
          postfix), 0)

        section.intermediateEncodables(reference) should be(Nil)
      }
    }

    "queried for a references direction" should {

      "know when a indirect reference is a forward reference" in {
        val label = Label.unique
        val reference: RelativeReference = mock[RelativeReference]
        (reference.target _).expects().returning(label).anyNumberOfTimes()
        (reference.label _).expects().returning(NoLabel()).anyNumberOfTimes()
        val target = EncodedByteList(0.toByte :: Nil)(label)

        val section = Section(List[Resource](
          reference,
          target), 0)

        section.isForwardReference(reference) should be(true)
      }

      "know when a indirect reference is a backward reference" in {
        val label = Label.unique
         val reference: RelativeReference = mock[RelativeReference]
        (reference.target _).expects().returning(label).anyNumberOfTimes()
        (reference.label _).expects().returning(NoLabel()).anyNumberOfTimes()
        val target = EncodedByteList(0.toByte :: Nil)(label)

        val section = Section(List[Resource](
          target,
          reference), 0)

        section.isForwardReference(reference) should be(false)
      }
    }

    "asked to encode itself" should {

      "be able to encode itself" in {
        val section = Section(List[Resource](
          EncodedByteList(0x00.toByte :: 0x01.toByte :: Nil),
          EncodedByteList(0xEF.toByte :: 0xFF.toByte :: Nil)), 0)

        section.encodable(application).encodeByte should be(0x00.toByte :: 0x01.toByte :: 0xEF.toByte :: 0xFF.toByte :: Nil)
      }
    }

    "queried for its size" should {

      "correctly calculate its size" in {
        val oneSize = 4
        val twoSize = 6
        val one = EncodedByteList(List.fill(oneSize)(1))
        val two = EncodedByteList(List.fill(twoSize)(2))

        val section = Section(List[Resource](
          one,
          two), 0)

        section.encodable(application).size should be(oneSize + twoSize)
      }
    }

    "queried for the relative address of a label" should {

      "correctly provide the section relative address of a label " in {
        val label = Label.unique
        val intermediate = EncodedByteList(List.fill(5)(0))
        val target = EncodedByteList(0.toByte :: Nil)(label)

        val section = Section(List[Resource](
          intermediate,
          target), 0)

        section.encodable(application).relativeAddress(target) should be(5)
      }
    }
  }
}