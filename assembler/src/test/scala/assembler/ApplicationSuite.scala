package assembler

import assembler.output.raw.Raw
import assembler.reference.RelativeReference
import assembler.sections.{AlignmentFiller, LastIteration, Section, SectionType}
import org.scalatest.{Matchers, WordSpec}

import scala.reflect.ClassTag

class ApplicationSuite extends WordSpec with Matchers {

  "an Application" when {

    def filler(count: Int): EncodedByteList =
      EncodedByteList(Seq.fill(count)(0x00.toByte))

    def encodables[T <: Encodable : ClassTag](content: List[Resource], references: Seq[Reference]): Map[DependentResource, T] = {
      val application = Raw(Section(SectionType.Text, "Text", content), 0)

      val result = application.encodablesForReferences(references)
      result.foreach(_._2 shouldBe a[T])
      result.mapValues(_.asInstanceOf[T])
    }

    "asked to return encodables for relative references with a linear sizeForDistance function" should {


      def myEncodables(content: List[Resource], references: Seq[Reference]) =
        encodables[LinearRelativeTestEncodable](content, references)

      "calculate the correct distance and size for a forward relative reference with a nearby target" in {
        val (reference, target) = TestEncodable.linearReferenceWithTarget
        val content: List[Resource] =
          reference ::
            filler(5) ::
            target ::
            Nil

        val result = myEncodables(content, Seq(reference))
        result(reference).distance shouldBe 5
        result(reference).size shouldBe 1
      }

      "calculate the correct distance and size for a backward relative reference with a farther target" in {
        val (reference, target) = TestEncodable.linearReferenceWithTarget
        val content: List[Resource] =
          target ::
            filler(15) ::
            reference ::
            Nil

        val result = myEncodables(content, Seq(reference))
        result(reference).distance shouldBe 16
        result(reference).size shouldBe 2
      }

      "represent a forward reference by a forward representation" in {
        val (reference, target) = TestEncodable.linearReferenceWithTarget
        val content: List[Resource] =
          reference ::
            filler(5) ::
            target ::
            Nil

        val result = myEncodables(content, Seq(reference))
        result(reference).offsetDirection shouldBe OffsetDirection.Forward
      }

      "represent a backward reference by a backward representation" in {
        val (reference, target) = TestEncodable.linearReferenceWithTarget
        val content: List[Resource] =
          target ::
            filler(5) ::
            reference ::
            Nil

        val result = myEncodables(content, Seq(reference))
        result(reference).offsetDirection shouldBe OffsetDirection.Backward
      }

      "represent two forward references with a nearby target where one depends on the other for its size" in {
        val (reference1, target1) = TestEncodable.linearReferenceWithTarget
        val (reference2, target2) = TestEncodable.linearReferenceWithTarget
        val content: List[Resource] =
          reference1 ::
            filler(2) ::
            reference2 ::
            filler(2) ::
            target1 ::
            filler(2) ::
            target2 ::
            Nil

        val result = myEncodables(content, Seq(reference1, reference2))
        result(reference1).distance shouldBe 5
        result(reference2).distance shouldBe 5
      }

      "represent two forward references with a farther target where one depends on the other for its size" in {
        val (reference1, target1) = TestEncodable.linearReferenceWithTarget
        val (reference2, target2) = TestEncodable.linearReferenceWithTarget
        val content: List[Resource] =
          reference1 ::
            filler(2) ::
            reference2 ::
            filler(12) ::
            target1 ::
            filler(2) ::
            target2 ::
            Nil

        val result = myEncodables(content, Seq(reference1, reference2))
        result(reference1).distance shouldBe 16
        result(reference2).distance shouldBe 15
      }

      "represent two references with a farther target which both depend on each other for their size where there is an obvious single resolution" in {
        val (reference1, target1) = TestEncodable.linearReferenceWithTarget
        val (reference2, target2) = TestEncodable.linearReferenceWithTarget
        val content: List[Resource] =
          target2 ::
            filler(12) ::
            reference1 ::
            filler(12) ::
            reference2 ::
            filler(2) ::
            target1 ::
            Nil

        val result = myEncodables(content, Seq(reference1, reference2))
        result(reference1).distance shouldBe 17
        result(reference2).distance shouldBe 27
      }

      "represent two references with a farther target which both depend on each other for their size where there are multiple resolutions from which the optimal (smallest) one should be chosen" in {
        val (reference1, target1) = TestEncodable.linearReferenceWithTarget
        val (reference2, target2) = TestEncodable.linearReferenceWithTarget
        val content: List[Resource] =
          target2 ::
            filler(4) ::
            reference1 ::
            filler(12) ::
            reference2 ::
            filler(5) ::
            target1 ::
            Nil

        val result = myEncodables(content, Seq(reference1, reference2))
        result(reference1).distance shouldBe 19
        result(reference2).distance shouldBe 19
      }
    }

    "asked to return encodables for relative references with a non-linear sizeForDistance function" should {

     def myEncodables(content: List[Resource], references: Seq[Reference]) =
        encodables[NonLinearRelativeTestEncodable](content, references)

      "represent two references with a farther target which both depend on each other for their size where there is an obvious single resolution" in {
        val (reference1, target1) = TestEncodable.nonLinearReferenceWithTarget
        val (reference2, target2) = TestEncodable.nonLinearReferenceWithTarget
        val content: List[Resource] =
          target2 ::
            filler(12) ::
            reference1 ::
            filler(12) ::
            reference2 ::
            filler(2) ::
            target1 ::
            Nil

        val result = myEncodables(content, Seq(reference1, reference2))
        result(reference1).distance shouldBe 16
        result(reference2).distance shouldBe 28
      }

      // TODO the algorithm should be modified to be able to resolve this case (i.e. Insert NOPs).
      "(currently) not represent two references with a farther target which both depend on each other for their size where there is no optimal resolution" in {
        val (reference1, target1) = TestEncodable.nonLinearReferenceWithTarget
        val (reference2, target2) = TestEncodable.nonLinearReferenceWithTarget
        val content: List[Resource] =
          target2 ::
            filler(14) ::
            reference1 ::
            filler(2) ::
            reference2 ::
            filler(5) ::
            target1 ::
            Nil

        an[AssertionError] shouldBe thrownBy { myEncodables(content, Seq(reference1, reference2)) }
      }
    }

    "defined with multiple sections" should {
      case class MyApplication(override val sections: List[Section], override val startOffset: Int) extends Application(sections) {
        override def encodeByte: List[Byte] = ???
      }

      "align the first section" when {
        val section = Section(SectionType.Data, "Test", List(EncodedByteList(Seq(0x00.toByte))))

        "there is a zero start offset" in {
          val application = MyApplication(List(section), 0)
          val first = application.encodableSections.head.finalContent.head
          first shouldBe a[EncodedByteList]
          val filler = first.asInstanceOf[EncodedByteList]
          filler.size shouldBe 0
        }

        "there is a start offset of 1" in {
          val application = MyApplication(List(section), 1)
          val resource = application.encodableSections.head.finalContent.head
          resource shouldBe a[EncodedByteList]
          val filler = resource.asInstanceOf[EncodedByteList]
          filler.size shouldBe 15
        }
      }

      "align the second section" when {
        val second = Section(SectionType.Data, "Second", List(EncodedByteList(Seq(0x02.toByte))))

        "there is a zero start offset and a 16 byte first section" in {
          val first = Section(SectionType.Data, "First", List(EncodedByteList(Seq.fill(16)(0x01.toByte))))
          val application = MyApplication(List(first, second), 0)

          val resource = application.encodableSections(1).finalContent.head
          resource shouldBe a[EncodedByteList]
          val filler = resource.asInstanceOf[EncodedByteList]
          filler.size shouldBe 0
        }

        "there is a zero start offset and a 20 byte first section" in {
          val first = Section(SectionType.Data, "First", List(EncodedByteList(Seq.fill(20)(0x01.toByte))))
          val application = MyApplication(List(first, second), 0)

          val resource = application.encodableSections(1).finalContent.head
          resource shouldBe a[EncodedByteList]
          val filler = resource.asInstanceOf[EncodedByteList]
          filler.size shouldBe 12
        }
      }

      "asked for the intermediate resources" should {
        def dummyResource = EncodedByteList(Seq(0xdd.toByte))

        val (relative1, targetRelative1) = TestEncodable.linearReferenceWithTarget
        val (relative2, targetRelative2) = TestEncodable.linearReferenceWithTarget
        val (absolute1, targetAbsolute1) = TestEncodable.absoluteReferenceWithTarget
        val (absolute2, targetAbsolute2) = TestEncodable.absoluteReferenceWithTarget
        val dummy1in1 = dummyResource
        val dummy2in1 = dummyResource
        val dummy1in2 = dummyResource
        val dummy2in2 = dummyResource

        val first = Section(SectionType.Text, "First", List(relative1, absolute1, dummy1in1, targetAbsolute2, targetRelative1, dummy2in1))
        val second = Section(SectionType.Text, "Second", List(absolute2, dummy1in2, targetRelative2, dummy2in2, relative2, targetAbsolute1))

        val filler1 = first.content.head
        val filler2 = second.content.head

        val application: MyApplication = MyApplication(List(first, second), 100)

        "return the intermediate resources for a relative reference in the first section" in {
          application.intermediateResources(relative1) shouldBe (Seq(absolute1, dummy1in1, targetAbsolute2), OffsetDirection.Forward)
        }

        "return the intermediate resources for a relative reference in the second section" in {
          application.intermediateResources(relative2) shouldBe (Seq(targetRelative2, dummy2in2), OffsetDirection.Backward)
        }

        "return the intermediate resources for an absolute reference in the first section" in {
          application.intermediateResources(absolute1) shouldBe (Seq(filler1, relative1, absolute1, dummy1in1,
            targetAbsolute2, targetRelative1, dummy2in1, filler2, absolute2, dummy1in2, targetRelative2, dummy2in2, relative2), OffsetDirection.Absolute)
        }

        "return the intermediate resources for an absolute reference in the second section" in {
          application.intermediateResources(absolute2) shouldBe (Seq(filler1, relative1, absolute1, dummy1in1), OffsetDirection.Absolute)
        }
      }
    }
  }
}
