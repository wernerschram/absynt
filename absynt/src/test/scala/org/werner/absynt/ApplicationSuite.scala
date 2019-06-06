/*
 * Copyright 2019 Werner Schram
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use
 * this file except in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */

package org.werner.absynt

import org.werner.absynt.output.raw.Raw
import org.werner.absynt.resource._
import org.werner.absynt.sections.Section
import org.scalatest.{Matchers, WordSpec}

import scala.reflect.ClassTag

class ApplicationSuite extends WordSpec with Matchers {

  "an Application" when {

    def filler(count: Int): EncodedBytes =
      EncodedBytes(Seq.fill(count)(0x00.toByte))

    def encodables[T <: UnlabeledEncodable : ClassTag](content: Seq[Resource], references: Seq[DependentResource]): Map[DependentResource, T] = {
      val application = Raw(Section.text(content), 0)

      val result = application.encodablesForDependencies(references)
      result.foreach(_._2 shouldBe a[T])
      result.mapValues(_.asInstanceOf[T])
    }

    "asked to return encodables for relative references with a linear sizeForDistance function" should {


      def myEncodables(content: Seq[Resource], references: Seq[DependentResource]) =
        encodables[LinearRelativeTestEncodable](content, references)

      "calculate the correct distance and size for a forward relative reference with a nearby target" in {
        val (reference, target) = TestEncodable.linearReferenceWithTarget
        val content: Seq[Resource] =
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
        val content: Seq[Resource] =
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
        val content: Seq[Resource] =
          reference ::
          filler(5) ::
          target ::
          Nil

        val result = myEncodables(content, Seq(reference))
        result(reference).offsetDirection shouldBe OffsetDirection.Forward
      }

      "represent a backward reference by a backward representation" in {
        val (reference, target) = TestEncodable.linearReferenceWithTarget
        val content: Seq[Resource] =
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
        val content: Seq[Resource] =
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
        val content: Seq[Resource] =
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
        val content: Seq[Resource] =
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
        val content: Seq[Resource] =
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

      "be able to calculate a reference to an empty virtual instruction (for example a meta assembler instruction which represents a NOP and results in no instruction)" in {
        val label: Label = Label.unique
        val target = EncodableCollection(Nil).label(label)
        val reference = LinearRelativeTestReference(label)

        val content: Seq[Resource] =
          reference ::
          filler(2) ::
          target ::
          filler(2) ::
          Nil

        val application = Raw(Section.text(content), 0)
        val result = application.encodablesForDependencies(Seq(reference))

        result(reference).asInstanceOf[LinearRelativeTestEncodable].distance shouldBe 2
        target.encodeByte shouldBe Nil
        application.encodeByte shouldBe Hex.lsb("ff 00 00 00 00")
      }
    }

    "asked to return encodables for relative references with a non-linear sizeForDistance function" should {

     def myEncodables(content: Seq[Resource], references: Seq[UnlabeledDependentResource]) =
        encodables[NonLinearRelativeTestEncodable](content, references)

      "represent two references with a farther target which both depend on each other for their size where there is an obvious single resolution" in {
        val (reference1, target1) = TestEncodable.nonLinearReferenceWithTarget
        val (reference2, target2) = TestEncodable.nonLinearReferenceWithTarget
        val content: Seq[Resource] =
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
        val content: Seq[Resource] =
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


    "defined with multiple sections" when {
      case class MyApplication(override val sections: Seq[Section], override val startOffset: Int) extends Application {
        def encodeByte: Seq[Byte] = Seq.empty

        override def alignmentFillers: Map[Section, AlignmentFiller] = sections.map(s => s -> AlignmentFiller(s)).toMap
      }

      "asked to return encodables for absolute references with a linear sizeForDistance function" should {

        "represent a reference that depends on the alignment of a section which depends on the size of the section the reference is in, where there is an obvious solution" in {
          val (reference, target) = TestEncodable.absoluteReferenceWithTarget
          val section1 = Section.text(
            reference ::
            filler(15) ::
            Nil)

          val section2 = Section.data(
            target :: Nil)

          val application = MyApplication(List(section1, section2), 0)
          val fillers = application.alignmentFillers
          val result = application.encodablesForDependencies(reference +: fillers.values.toSeq)
          val encodable = result(reference).asInstanceOf[AbsoluteTestEncodable]
          val alignment2 = result(fillers(section2)).asInstanceOf[EncodedBytes]

          encodable.size shouldBe 3
          alignment2.size shouldBe 14
        }
      }

      "asked for the size of an alignment filler" should {

        "align the first section" when {
          val section = Section.data(List(EncodedBytes(Seq(0x00.toByte))))

          "there is a zero start offset" in {
            val application = MyApplication(List(section), 0)
            val alignmentFiller = application.alignmentFillers(section)
            val filler = application.encodablesForDependencies(Seq(alignmentFiller))(alignmentFiller)
            filler.size shouldBe 0
          }

          "there is a start offset of 1" in {
            val application = MyApplication(List(section), 1)
            val alignmentFiller = application.alignmentFillers(section)
            val filler = application.encodablesForDependencies(Seq(alignmentFiller))(alignmentFiller)
            filler.size shouldBe 15
          }
        }

        "align the second section" when {
          val second = Section.data(List(EncodedBytes(Seq(0x02.toByte))), "Second")

          "there is a zero start offset and a 16 byte first section" in {
            val first = Section.data(List(EncodedBytes(Seq.fill(16)(0x01.toByte))), "First")
            val application = MyApplication(List(first, second), 0)

            val alignmentFiller = application.alignmentFillers(second)
            val filler = application.encodablesForDependencies(Seq(alignmentFiller))(alignmentFiller)
            filler.size shouldBe 0
          }

          "there is a zero start offset and a 20 byte first section" in {
            val first = Section.data(List(EncodedBytes(Seq.fill(20)(0x01.toByte))), "First")
            val application = MyApplication(List(first, second), 0)

            val alignmentFiller = application.alignmentFillers(second)
            val filler = application.encodablesForDependencies(Seq(alignmentFiller))(alignmentFiller)
            filler.size shouldBe 12
          }
        }
      }

      "asked for the intermediate resources" should {
        def dummyResource = EncodedBytes(Seq(0xdd.toByte))

        val (relative1, targetRelative1) = TestEncodable.linearReferenceWithTarget
        val (relative2, targetRelative2) = TestEncodable.linearReferenceWithTarget
        val (absolute1, targetAbsolute1) = TestEncodable.absoluteReferenceWithTarget
        val (absolute2, targetAbsolute2) = TestEncodable.absoluteReferenceWithTarget
        val dummy1in1 = dummyResource
        val dummy2in1 = dummyResource
        val dummy1in2 = dummyResource
        val dummy2in2 = dummyResource

        val first = Section.text(List(relative1, absolute1, dummy1in1, targetAbsolute2, targetRelative1, dummy2in1), "First")
        val second = Section.text(List(absolute2, dummy1in2, targetRelative2, dummy2in2, relative2, targetAbsolute1), "Second")

        val application: MyApplication = MyApplication(List(first, second), 100)

        val startFiller = application.startFiller

        val filler1 = application.alignmentFillers(first)
        val filler2 = application.alignmentFillers(second)

        "return the intermediate resources for a relative reference in the first section" in {
          relative1.dependencies(application) shouldBe (Seq(absolute1, dummy1in1, targetAbsolute2), OffsetDirection.Forward)
        }

        "return the intermediate resources for a relative reference in the second section" in {
          relative2.dependencies(application) shouldBe (Seq(targetRelative2, dummy2in2), OffsetDirection.Backward)
        }

        "return the intermediate resources for an absolute reference in the first section" in {
          absolute1.dependencies(application) shouldBe (Seq(startFiller, filler1, relative1, absolute1, dummy1in1,
            targetAbsolute2, targetRelative1, dummy2in1, filler2, absolute2, dummy1in2, targetRelative2, dummy2in2, relative2), OffsetDirection.Absolute)
        }

        "return the intermediate resources for an absolute reference in the second section" in {
          absolute2.dependencies(application) shouldBe (Seq(startFiller, filler1, relative1, absolute1, dummy1in1), OffsetDirection.Absolute)
        }

        "return the intermediate resources for the filler in the first section" in {
          filler1.dependencies(application) shouldBe(Seq(startFiller), OffsetDirection.Absolute)
        }

        "return the intermediate resources for the filler in the second section" in {
          filler2.dependencies(application) shouldBe (Seq(startFiller, filler1, relative1, absolute1, dummy1in1,
            targetAbsolute2, targetRelative1, dummy2in1), OffsetDirection.Absolute)
        }
      }
    }
  }
}
