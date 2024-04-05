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

import org.werner.absynt.Hex
import org.werner.absynt.arm.ProcessorMode
import org.werner.absynt.arm.operands.Condition
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MultiplySuite extends AnyWordSpec with Matchers {

  "an MultiplyAccumulate instruction" when {
    "in a32 mode" should {

      import ProcessorMode.A32.{given, *}

      "correctly encode mlagt r1, r3, r2, r4" in {
        MultiplyAccumulate(R1, R2, R3, R4, Condition.SignedGreaterThan).encodeByte should be(Hex.msb("c0214293"))
      }

      "correctly represent mlagt r1, r3, r2, r4 as a string" in {
        MultiplyAccumulate(R1, R2, R3, R4, Condition.SignedGreaterThan).toString should be("mlagt r1, r3, r2, r4")
      }

      "correctly encode mlasvs r1, r3, r2, r4" in {
        MultiplyAccumulate.setFlags(R1, R2, R3, R4, Condition.Overflow).encodeByte should be(Hex.msb("60314293"))
      }
    }
  }

  "an Multiply instruction" when {
    "in a32 mode" should {

      import ProcessorMode.A32.{given, *}

      "correctly encode mulgt r1, r3, r2" in {
        Multiply(R1, R2, R3, Condition.SignedGreaterThan).encodeByte should be(Hex.msb("c0010293"))
      }

      "correctly represent mulgt r1, r3, r2 as a string" in {
        Multiply(R1, R2, R3, Condition.SignedGreaterThan).toString should be("mulgt r1, r3, r2")
      }

      "correctly encode mulsvs r1, r3, r2" in {
        Multiply.setFlags(R1, R2, R3, Condition.Overflow).encodeByte should be(Hex.msb("60110293"))
      }

    }
  }
}