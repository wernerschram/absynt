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
    }
  }
}
