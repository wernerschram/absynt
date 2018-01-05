package assembler.sections

import assembler._
import assembler.output.raw.Raw
import assembler.resource.{Encodable, RelativeReference, Resource}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import assembler.resource.EncodableConversion._

class SectionSuite extends WordSpec with Matchers with MockFactory {

  "a Section" when {

    class MyReference(targetLabel: Label, label: Label) extends RelativeReference(targetLabel, label) {
      override def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int = ???
      override def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): Encodable = ???
      override def possibleSizes: Set[Int] = ???
    }

    "queried for immediate instructions" should {

      "provide the intermediate instructions between a relative instruction and a label" in {
        val targetLabel = Label.unique
        val reference = new MyReference(targetLabel, Label.noLabel)
        val intermediate = EncodedByteList(List.fill(5)(0))
        val target = EncodedByteList(0.toByte :: Nil)(targetLabel)

        val section = Section(SectionType.Text, ".test", List[Resource](
          reference,
          intermediate,
          target))

        section.intermediateResources(reference) should be(intermediate :: Nil)
      }

      "provide the intermediate instructions between a label and a relative instruction" in {
        val targetLabel = Label.unique
        val reference = new MyReference(targetLabel, Label.noLabel)
        val intermediate = EncodedByteList(List.fill(5)(0))
        val target = EncodedByteList(0.toByte :: Nil)(targetLabel)

        val section = Section(SectionType.Text, ".test", List[Resource](
          target,
          intermediate,
          reference))

        section.intermediateResources(reference) should be(target :: intermediate :: Nil)
      }

      "return an empty list for an instruction that references itself" in {
        val targetLabel = Label.unique
        val reference = new MyReference(targetLabel, targetLabel)
        val prefix = EncodedByteList(List.fill(2)(0))
        val postfix = EncodedByteList(List.fill(3)(0))

        val section = Section(SectionType.Text, ".test", List[Resource](
          prefix,
          reference,
          postfix))

        section.intermediateResources(reference) should be(Nil)
      }
    }

    "queried for a references direction" should {

      "know when a indirect reference is a forward reference" in {
        val targetLabel = Label.unique
        val reference = new MyReference(targetLabel, Label.noLabel)
        val target = EncodedByteList(0.toByte :: Nil)(targetLabel)

        val section = Section(SectionType.Text, ".test", List[Resource](
          reference,
          target))

        section.offsetDirection(reference) shouldBe OffsetDirection.Forward
      }

      "know when a indirect reference is a backward reference" in {
        val targetLabel = Label.unique
        val reference = new MyReference(targetLabel, Label.noLabel)
        val target = EncodedByteList(0.toByte :: Nil)(targetLabel)

        val section = Section(SectionType.Text, ".test", List[Resource](
          target,
          reference))

        section.offsetDirection(reference) shouldBe OffsetDirection.Backward
      }

      "know when a indirect reference is a reference to self" in {
        val targetLabel = Label.unique
        val reference = new MyReference(targetLabel, targetLabel)

        val section = Section(SectionType.Text, ".test", List[Resource](
          reference))

        section.offsetDirection(reference) shouldBe OffsetDirection.Self
      }

    }

    "asked to encode itself" should {

      "be able to encode itself" in {
        val section = Section(SectionType.Text, ".test", List[Resource](
          EncodedByteList(0x00.toByte :: 0x01.toByte :: Nil),
          EncodedByteList(0xEF.toByte :: 0xFF.toByte :: Nil)))

        val application: Application = Raw(section, 0)
        section.content.encodables(application.encodablesForReferences(section.content.dependentResources))
          .encodeByte should be(0x00.toByte :: 0x01.toByte :: 0xEF.toByte :: 0xFF.toByte :: Nil)
      }
    }

    "queried for its size" should {

      "correctly calculate its size" in {
        val oneSize = 4
        val twoSize = 6
        val one = EncodedByteList(List.fill(oneSize)(1))
        val two = EncodedByteList(List.fill(twoSize)(2))

        val section = Section(SectionType.Text, ".test", List[Resource](
          one,
          two))

        val application: Application = Raw(section, 0)
        section.content.encodables(application.encodablesForReferences(section.content.dependentResources))
          .encodeByte.length should be(oneSize + twoSize)
      }
    }
  }
}