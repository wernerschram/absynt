package assembler

import assembler.output.raw.Raw
import assembler.reference.RelativeReference
import assembler.sections.{Section, SectionType}
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

      case class MyEncodable(distance: Int, offsetDirection: RelativeOffsetDirection, override val label: Label) extends Encodable {
        override def encodeByte: Seq[Byte] =
          offsetDirection match {
            case OffsetDirection.Forward => Seq.fill(size)(0xff.toByte)
            case OffsetDirection.Backward => Seq.fill(size)(0xbb.toByte)
            case OffsetDirection.Self => Seq.fill(size)(0x88.toByte)
          }

        override def size: Int =
          if (distance < 10) 1
          else if (distance < 20) 2
          else 3
      }

      case class MyRelativeReference(override val target: Label, override val label: Label = Label.noLabel) extends RelativeReference {
        override def encodeForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): Encodable =
          MyEncodable(distance, offsetDirection, label)

        override def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int =
          if (dependencySize < 10) 1
          else if (dependencySize < 20) 2
          else 3

        override def possibleSizes = Set(1, 2, 3)
      }

      def referenceWithTarget = {
        val targetLabel = Label.unique
        val reference = MyRelativeReference(targetLabel)
        val targetResource = EncodedByteList(Seq(0x00.toByte))(targetLabel)
        (reference, targetResource)
      }

      def myEncodables(content: List[Resource], references: Seq[Reference]) =
        encodables[MyEncodable](content, references)

      "calculate the correct distance and size for a forward relative reference with a nearby target" in {
        val (reference, target) = referenceWithTarget
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
        val (reference, target) = referenceWithTarget
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
        val (reference, target) = referenceWithTarget
        val content: List[Resource] =
          reference ::
            filler(5) ::
            target ::
            Nil

        val result = myEncodables(content, Seq(reference))
        result(reference).offsetDirection shouldBe OffsetDirection.Forward
      }

      "represent a backward reference by a backward representation" in {
        val (reference, target) = referenceWithTarget
        val content: List[Resource] =
          target ::
            filler(5) ::
            reference ::
            Nil

        val result = myEncodables(content, Seq(reference))
        result(reference).offsetDirection shouldBe OffsetDirection.Backward
      }

      "represent two forward references with a nearby target where one depends on the other for its size" in {
        val (reference1, target1) = referenceWithTarget
        val (reference2, target2) = referenceWithTarget
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
        val (reference1, target1) = referenceWithTarget
        val (reference2, target2) = referenceWithTarget
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
        val (reference1, target1) = referenceWithTarget
        val (reference2, target2) = referenceWithTarget
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
        val (reference1, target1) = referenceWithTarget
        val (reference2, target2) = referenceWithTarget
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

      case class MyEncodable(distance: Int, offsetDirection: RelativeOffsetDirection, override val label: Label) extends Encodable {
        override def encodeByte: Seq[Byte] =
          offsetDirection match {
            case OffsetDirection.Forward => Seq.fill(size)(0xff.toByte)
            case OffsetDirection.Backward => Seq.fill(size)(0xbb.toByte)
            case OffsetDirection.Self => Seq.fill(size)(0x88.toByte)
          }

        override def size: Int =
          if (distance < 10) 1
          else if (distance < 20) 3
          else 2
      }

      case class MyRelativeReference(override val target: Label, override val label: Label = Label.noLabel) extends RelativeReference {
        override def encodeForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): Encodable =
          MyEncodable(distance, offsetDirection, label)

        override def sizeForDependencySize(dependencySize: Int, offsetDirection: OffsetDirection): Int =
          if (dependencySize < 10) 1
          else if (dependencySize < 20) 3
          else 2

        override def possibleSizes = Set(1, 2, 3)
      }

      def referenceWithTarget = {
        val targetLabel = Label.unique
        val reference = MyRelativeReference(targetLabel)
        val targetResource = EncodedByteList(Seq(0x00.toByte))(targetLabel)
        (reference, targetResource)
      }

      def myEncodables(content: List[Resource], references: Seq[Reference]) =
        encodables[MyEncodable](content, references)

      "represent two references with a farther target which both depend on each other for their size where there is an obvious single resolution" in {
        val (reference1, target1) = referenceWithTarget
        val (reference2, target2) = referenceWithTarget
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
        val (reference1, target1) = referenceWithTarget
        val (reference2, target2) = referenceWithTarget
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

  }
}
