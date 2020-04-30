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

package org.werner.absynt.arm.instructions

import org.werner.absynt._
import org.werner.absynt.arm.operands._
import org.werner.absynt.arm.ProcessorMode
import org.werner.absynt.output.raw.Raw
import org.werner.absynt.resource.Resource
import org.werner.absynt.sections.Section
import org.scalatest.{Matchers, WordSpec}

class BranchSuite extends WordSpec with Matchers {

  "an Branch instruction" when {
    "in a32 mode" should {

      import ProcessorMode.A32._


      "correctly encode b +0x3e8" in {
        Branch(RelativeA32Pointer(ArmRelativeOffset(0x3e8))).encodeByte should be(Hex.msb("ea0000fa"))
      }

      "correctly encode beq +0x1111110" in {
        Branch(RelativeA32Pointer(ArmRelativeOffset(0x1111110)), Equal).encodeByte should be(Hex.msb("0a444444"))
      }

      "correctly encode blt -0x08" in {
        Branch(RelativeA32Pointer(ArmRelativeOffset(-0x08)), SignedLessThan).encodeByte should be(Hex.msb("bafffffe"))
      }

      "correctly represent blt -0x08 as a string" in {
        Branch(RelativeA32Pointer(ArmRelativeOffset(-0x08)), SignedLessThan).toString should be("blt 0x00000000")
      }

      "correctly encode a forward branch to a labeled instruction" in {
        val targetLabel = Label.unique
        val reference = Branch(targetLabel)
        val p = Section.text(List[Resource](
          reference,
            EncodedBytes.fill(4, 0x00.toByte),
            EncodedBytes.fill(4, 0x00.toByte).label(targetLabel)
        ))

        val application: Application = Raw(p, 0)
        val encodables = application.encodablesForDependencies(Seq(reference))
        encodables(reference).encodeByte shouldBe Hex.msb("EA000000")
      }

      "correctly encode a backward branch to a labeled instruction" in {
        val targetLabel: Label = "Label"
        val reference = Branch(targetLabel, LowerOrSame)
        val p = Section.text(List[Resource](
          EncodedBytes.fill(4, 0x00.toByte).label(targetLabel),
          EncodedBytes.fill(4, 0x00.toByte),
          reference))

        val application: Application = Raw(p, 0)
        val encodables = application.encodablesForDependencies(Seq(reference))
        encodables(reference).encodeByte shouldBe Hex.msb("9AFFFFFC")
      }

      "correctly encode a branch to self instruction" in {
        val targetLabel = Label.unique
        val reference = Branch(targetLabel).label(targetLabel)
        val p = Section.text(List[Resource](
          EncodedBytes.fill(8, 0x00.toByte),
          reference,
          EncodedBytes.fill(8, 0x00.toByte)))

        val application: Application = Raw(p, 0)
        val encodables = application.encodablesForDependencies(Seq(reference))
        encodables(reference).encodeByte shouldBe Hex.msb("EAFFFFFE")
      }

      "correctly encode a forward branch over another branch to a labeled instruction" in {
        val targetLabel = Label.unique
        val reference = Branch(targetLabel)
        val p = Section.text(List[Resource](
          Branch(targetLabel),
          EncodedBytes.fill(4, 0x00.toByte),
          reference,
          EncodedBytes.fill(4, 0x00.toByte),
          EncodedBytes.fill(4, 0x00.toByte).label(targetLabel)
        ))

        val application: Application = Raw(p, 0)
        val encodables = application.encodablesForDependencies(Seq(reference))
        encodables(reference).encodeByte shouldBe Hex.msb("EA000000")
      }

      "correctly encode a backward branch over another branch to a labeled instruction" in {
        val targetLabel = Label.unique
        val reference1 = Branch(targetLabel)
        val reference2 = Branch(targetLabel)
        val p = Section.text(List[Resource](
          EncodedBytes.fill(4, 0x00.toByte).label(targetLabel),
          EncodedBytes.fill(4, 0x00.toByte),
          reference1,
          EncodedBytes.fill(4, 0x00.toByte),
          reference2
        ))

        val application: Application = Raw(p, 0)
        val encodables = application.encodablesForDependencies(Seq(reference1, reference2))
        encodables(reference1).encodeByte shouldBe Hex.msb("EAFFFFFC")
        encodables(reference2).encodeByte shouldBe Hex.msb("EAFFFFFA")
      }

      "correctly represent b Label as a string" in {
        val targetLabel: Label = "Label"
        Branch(targetLabel).toString should be("b Label")
      }

      "correctly represent bne Label as a string" in {
        val targetLabel: Label = "Label"
        Branch(targetLabel, Condition.NotEqual).toString should be("bne Label")
      }
    }
  }

  "an BranchLink instruction" when {
    "in a32 mode" should {

      import ProcessorMode.A32._

      "correctly encode bleq 0x1111118" in {
        BranchLink(RelativeA32Pointer(ArmRelativeOffset(0x1111110)), Equal).encodeByte should be(Hex.msb("0b444444"))
      }

      "correctly represent bleq 0x01111118 as a string" in {
        BranchLink(RelativeA32Pointer(ArmRelativeOffset(0x1111110)), Equal).toString should be("bleq 0x01111118")
      }

      "correctly encode a forward branch-link to a labeled instruction" in {
        val targetLabel = Label.unique
        val reference = BranchLink(targetLabel)
        val p = Section.text(List[Resource](
          reference,
          EncodedBytes.fill(4, 0x00.toByte),
          EncodedBytes.fill(4, 0x00.toByte).label(targetLabel)
        ))

        val application: Application = Raw(p, 0)
        val encodables = application.encodablesForDependencies(Seq(reference))
        encodables(reference).encodeByte shouldBe Hex.msb("EB000000")
      }
    }
  }

  "an BranchLinkExchange instruction" when {
    "in a32 mode" should {

      import ProcessorMode.A32._

      "correctly encode blx 0x123C" in {
        BranchLinkExchange(RelativeThumbPointer(ArmRelativeOffset(0x1234))).encodeByte should be(Hex.msb("fa00048d"))
      }

      "correctly represent blx 0x123C as a string" in {
        BranchLinkExchange(RelativeThumbPointer(ArmRelativeOffset(0x1234))).toString should be("blx 0x0000123C")
      }

      "correctly encode blx 0x123E" in {
        BranchLinkExchange(RelativeThumbPointer(ArmRelativeOffset(0x1236))).encodeByte should be(Hex.msb("fb00048d"))
      }

      "correctly represent blx 0x123E as a string" in {
        BranchLinkExchange(RelativeThumbPointer(ArmRelativeOffset(0x1236))).toString should be("blx 0x0000123E")
      }

      "correctly encode blx r12" in {
        BranchLinkExchange(R12).encodeByte should be(Hex.msb("e12fff3c"))
      }

      "correctly encode a forward branch-link-exchange to a labeled instruction" in {
        val targetLabel = Label.unique
        val reference = BranchLinkExchange(targetLabel)
        val p = Section.text(List[Resource](
          reference,
          EncodedBytes.fill(4, 0x00.toByte),
          EncodedBytes.fill(4, 0x00.toByte).label(targetLabel)
        ))

        val application: Application = Raw(p, 0)
        val encodables = application.encodablesForDependencies(Seq(reference))
        encodables(reference).encodeByte shouldBe Hex.msb("FA000000")
      }
    }
  }

  "a BranchExchange instruction" when {
    "in a32 mode" should {

      import ProcessorMode.A32._

      "correctly encode bx r1" in {
        BranchExchange(R1).encodeByte should be(Hex.msb("e12fff11"))
      }

      "correctly represent bx r1 as a string" in {
        BranchExchange(R1).toString should be("bx r1")
      }

    }
  }

  "a BranchExchangeJazelle instruction" when {
    "in a32 mode" should {

      import ProcessorMode.A32._

      "correctly encode bxj r2" in {
        BranchExchangeJazelle(R2).encodeByte should be(Hex.msb("e12fff22"))
      }

      "correctly represent bxj r2 as a string" in {
        BranchExchangeJazelle(R2).toString should be("bxj r2")
      }
    }
  }
}