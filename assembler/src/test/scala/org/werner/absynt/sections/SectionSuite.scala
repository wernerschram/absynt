package org.werner.absynt.sections

import org.werner.absynt._
import org.werner.absynt.output.raw.Raw
import org.werner.absynt.resource.EncodableConversion._
import org.werner.absynt.resource.Resource
import org.scalatest.{Matchers, WordSpec}

class SectionSuite extends WordSpec with Matchers {

  "a Section" when {

    "queried for immediate instructions" should {

      "provide the intermediate instructions between a relative instruction and a label" in {
        val targetLabel = Label.unique
        val reference = LinearRelativeTestReference(targetLabel)
        val intermediate = EncodedBytes(Seq.fill(5)(0.toByte))
        val target = EncodedBytes(0.toByte :: Nil).label(targetLabel)

        val section = Section.text(List[Resource](
          reference,
          intermediate,
          target))

        section.intermediateResources(reference) should be(intermediate :: Nil)
      }

      "provide the intermediate instructions between a label and a relative instruction" in {
        val targetLabel = Label.unique
        val reference = LinearRelativeTestReference(targetLabel)
        val intermediate = EncodedBytes(List.fill(5)(0.toByte))
        val target = EncodedBytes(0.toByte :: Nil).label(targetLabel)

        val section = Section.text(List[Resource](
          target,
          intermediate,
          reference))

        section.intermediateResources(reference) should be(target :: intermediate :: Nil)
      }

      "return an empty list for an instruction that references itself" in {
        val targetLabel = Label.unique
        val reference = LinearRelativeTestReference(targetLabel)
        val referenceWithLabel = reference.label(targetLabel)
        val prefix = EncodedBytes(List.fill(2)(0.toByte))
        val postfix = EncodedBytes(List.fill(3)(0.toByte))

        val section = Section.text(List[Resource](
          prefix,
          referenceWithLabel,
          postfix))

        section.intermediateResources(reference) should be(Nil)
      }
    }

    "queried for a references direction" should {

      "know when a indirect reference is a forward reference" in {
        val targetLabel = Label.unique
        val reference = LinearRelativeTestReference(targetLabel)
        val target = EncodedBytes(0.toByte :: Nil).label(targetLabel)

        val section = Section.text(List[Resource](
          reference,
          target))

        section.offsetDirection(reference) shouldBe OffsetDirection.Forward
      }

      "know when a indirect reference is a backward reference" in {
        val targetLabel = Label.unique
        val reference = LinearRelativeTestReference(targetLabel)
        val target = EncodedBytes(0.toByte :: Nil).label(targetLabel)

        val section = Section.text(List[Resource](
          target,
          reference))

        section.offsetDirection(reference) shouldBe OffsetDirection.Backward
      }

      "know when a indirect reference is a reference to self" in {
        val targetLabel = Label.unique
        val reference = LinearRelativeTestReference(targetLabel)
        val referenceWithLabel = reference.label(targetLabel)

        val section = Section.text(List[Resource](
          referenceWithLabel))

        section.offsetDirection(reference) shouldBe OffsetDirection.Self
      }

    }

    "asked to encode itself" should {

      "be able to encode itself" in {
        val section = Section.text(List[Resource](
          EncodedBytes(0x00.toByte :: 0x01.toByte :: Nil),
          EncodedBytes(0xEF.toByte :: 0xFF.toByte :: Nil)))

        val application: Application = Raw(section, 0)
        section.content.encodables(application.encodablesForDependencies(section.content.dependentResources))
          .encodeByte should be(0x00.toByte :: 0x01.toByte :: 0xEF.toByte :: 0xFF.toByte :: Nil)
      }
    }

    "queried for its size" should {

      "correctly calculate its size" in {
        val oneSize = 4
        val twoSize = 6
        val one = EncodedBytes(List.fill(oneSize)(1.toByte))
        val two = EncodedBytes(List.fill(twoSize)(2.toByte))

        val section = Section.text(List[Resource](
          one,
          two))

        val application: Application = Raw(section, 0)
        section.content.encodables(application.encodablesForDependencies(section.content.dependentResources))
          .encodeByte.length should be(oneSize + twoSize)
      }
    }
  }
}