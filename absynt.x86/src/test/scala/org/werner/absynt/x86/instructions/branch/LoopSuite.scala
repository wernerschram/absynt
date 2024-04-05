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

package org.werner.absynt.x86.instructions.branch

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.werner.absynt._
import org.werner.absynt.output.raw.Raw
import org.werner.absynt.resource.Resource
import org.werner.absynt.sections.Section
import org.werner.absynt.x86.ProcessorMode

class LoopSuite extends AnyWordSpec with Matchers {
  "a Loop instruction" when {

    "in real mode" should {

      import ProcessorMode.Real.{given, *}

      "correctly encode loop 0x10" in {
        Loop(shortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("E2 10"))
      }
      "correctly represent loop 0x10 as a string" in {
        Loop(shortPointer(0x10.toByte)).toString should be("loop 0x10")
      }

      "correctly encode loope 0x10" in {
        Loop.Equal(shortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("E1 10"))
      }
      "correctly represent loope 0x10 as a string" in {
        Loop.Equal(shortPointer(0x10.toByte)).toString should be("loope 0x10")
      }

      "correctly encode loopne 0x10" in {
        Loop.NotEqual(shortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("E0 10"))
      }
      "correctly represent loopne 0x10 as a string" in {
        Loop.NotEqual(shortPointer(0x10.toByte)).toString should be("loopne 0x10")
      }

      "Encode a simple program with an indirect backward short loop instruction" in {
        val targetLabel = Label.unique
        val loop = Loop(targetLabel)

        val p = Section.text(List[Resource](
          EncodedBytes.fill(1, 0x00.toByte).label(targetLabel),
          EncodedBytes.fill(1, 0x00.toByte),
          loop
        ))

        val app = Raw(p, 0)
        val encodable = app.encodablesForDependencies(loop :: Nil)

        withClue("Loop") {
          encodable(loop).encodeByte should be(Hex.lsb("E2 FC"))
        }
      }

      "Encode a simple program with an indirect forward short loop instruction" in {
        val targetLabel = Label.unique
        val loop = Loop(targetLabel)

        val p = Section.text(List[Resource](
          loop,
          EncodedBytes.fill(120, 0x00.toByte),
          EncodedBytes.fill(1, 0x00.toByte).label(targetLabel)
        ))

        val app = Raw(p, 0)
        val encodable = app.encodablesForDependencies(loop :: Nil)

        withClue("Loop") {
          encodable(loop).encodeByte should be(Hex.lsb("E2 78"))
        }
      }

      "throw an AssertionError for a simple program with a too far indirect backward loop instruction" in {
        val targetLabel = Label.unique
        val loop = Loop(targetLabel)

        val p = Section.text(List[Resource](
          EncodedBytes.fill(1, 0x00.toByte).label(targetLabel),
          EncodedBytes.fill(256, 0x00.toByte),
          loop,
        ))

        val app = Raw(p, 0)

        an[AssertionError] should be thrownBy {
          app.encodablesForDependencies(loop :: Nil)(loop).encodeByte
        }
      }
    }

    "in protected mode" should {

      import ProcessorMode.Protected.{given, *}

      "Encode a simple program with an indirect backward short loope instruction" in {
        val targetLabel = Label.unique
        val loope = Loop.Equal(targetLabel)

        val p = Section.text(List[Resource](
          EncodedBytes.fill(1, 0x00.toByte).label(targetLabel),
          EncodedBytes.fill(1, 0x00.toByte),
          loope
        ))

        val app = Raw(p, 0)
        val encodable = app.encodablesForDependencies(loope :: Nil)

        withClue("Loop.Equal") {
          encodable(loope).encodeByte should be(Hex.lsb("E1 FC"))
        }
      }

      "Encode a simple program with an indirect forward short loope instruction" in {
        val targetLabel = Label.unique
        val loope = Loop.Equal(targetLabel)

        val p = Section.text(List[Resource](
          loope,
          EncodedBytes.fill(120, 0x00.toByte),
          EncodedBytes.fill(1, 0x00.toByte).label(targetLabel)
        ))

        val app = Raw(p, 0)
        val encodable = app.encodablesForDependencies(loope :: Nil)

        withClue("Loop.Equal") {
          encodable(loope).encodeByte should be(Hex.lsb("E1 78"))
        }
      }

      "throw an AssertionError for a simple program with a too far indirect backward loope instruction" in {
        val targetLabel = Label.unique
        val loope = Loop.Equal(targetLabel)

        val p = Section.text(List[Resource](
          EncodedBytes.fill(1, 0x00.toByte).label(targetLabel),
          EncodedBytes.fill(256, 0x00.toByte),
          loope,
        ))

        val app = Raw(p, 0)

        an[AssertionError] should be thrownBy {
          app.encodablesForDependencies(loope :: Nil)(loope).encodeByte
        }
      }
    }

    "in long mode" should {

      import ProcessorMode.Long.{given, *}

      "Encode a simple program with an indirect backward short loopne instruction" in {
        val targetLabel = Label.unique
        val loopne = Loop.NotEqual(targetLabel)

        val p = Section.text(List[Resource](
          EncodedBytes.fill(1, 0x00.toByte).label(targetLabel),
          EncodedBytes.fill(1, 0x00.toByte),
          loopne
        ))

        val app = Raw(p, 0)
        val encodable = app.encodablesForDependencies(loopne :: Nil)

        withClue("Loop.NotEqual") {
          encodable(loopne).encodeByte should be(Hex.lsb("E0 FC"))
        }
      }

      "Encode a simple program with an indirect forward short loopne instruction" in {
        val targetLabel = Label.unique
        val loopne = Loop.NotEqual(targetLabel)

        val p = Section.text(List[Resource](
          loopne,
          EncodedBytes.fill(120, 0x00.toByte),
          EncodedBytes.fill(1, 0x00.toByte).label(targetLabel)
        ))

        val app = Raw(p, 0)
        val encodable = app.encodablesForDependencies(loopne :: Nil)

        withClue("Loop.NotEqual") {
          encodable(loopne).encodeByte should be(Hex.lsb("E0 78"))
        }
      }

      "throw an AssertionError for a simple program with a too far indirect backward loopne instruction" in {
        val targetLabel = Label.unique
        val loopne = Loop.NotEqual(targetLabel)

        val p = Section.text(List[Resource](
          EncodedBytes.fill(1, 0x00.toByte).label(targetLabel),
          EncodedBytes.fill(256, 0x00.toByte),
          loopne,
        ))

        val app = Raw(p, 0)

        an[AssertionError] should be thrownBy {
          app.encodablesForDependencies(loopne :: Nil)(loopne).encodeByte
        }
      }
    }
  }
}