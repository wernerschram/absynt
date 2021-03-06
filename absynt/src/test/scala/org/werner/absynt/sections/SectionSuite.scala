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

package org.werner.absynt.sections

import org.werner.absynt._
import org.werner.absynt.output.raw.Raw
import org.werner.absynt.resource.EncodableConversion._
import org.werner.absynt.resource.Resource
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SectionSuite extends AnyWordSpec with Matchers {

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
        val intermediate = EncodedBytes.fill(5, 0.toByte)
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
        val prefix = EncodedBytes.fill(2, 0.toByte)
        val postfix = EncodedBytes.fill(3, 0.toByte)

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
        val one = EncodedBytes.fill(oneSize, 1.toByte)
        val two = EncodedBytes.fill(twoSize, 2.toByte)

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