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

package org.werner.absynt.x86.instructions

import org.scalatest.{Matchers, WordSpec}
import org.werner.absynt.Hex
import org.werner.absynt.x86.ProcessorMode

class AdjustSuite extends WordSpec with Matchers {

  "an Adjust instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode in aaa" in {
        AdjustAfterAddition.ascii(AL).encodeByte should be(Hex.lsb("37"))
      }
      "correctly represent aaa as a string" in {
        AdjustAfterAddition.ascii(AL).toString should be("aaa")
      }

      "correctly encode in daa" in {
        AdjustAfterAddition.decimal(AL).encodeByte should be(Hex.lsb("27"))
      }
      "correctly represent daa as a string" in {
        AdjustAfterAddition.decimal(AL).toString should be("daa")
      }

      "correctly encode in aad" in {
        AdjustBeforeDivide.ascii(AX).encodeByte should be(Hex.lsb("D5 0A"))
      }
      "correctly represent aad as a string" in {
        AdjustBeforeDivide.ascii(AX).toString should be("aad 10")
      }

      "correctly encode in aad 11" in {
        AdjustBeforeDivide.base(AX, 11.toByte).encodeByte should be(Hex.lsb("D5 0B"))
      }
      "correctly represent aad 11 as a string" in {
        AdjustBeforeDivide.base(AX, 11.toByte).toString should be("aad 11")
      }

      "correctly encode in aam" in {
        AdjustAfterMultiply.ascii(AX).encodeByte should be(Hex.lsb("D4 0A"))
      }
      "correctly represent aam as a string" in {
        AdjustAfterMultiply.ascii(AX).toString should be("aam 10")
      }

      "correctly encode in aam 11" in {
        AdjustAfterMultiply.base(AX, 11.toByte).encodeByte should be(Hex.lsb("D4 0B"))
      }
      "correctly represent aam 11 as a string" in {
        AdjustAfterMultiply.base(AX, 11.toByte).toString should be("aam 11")
      }

      "correctly encode in aas" in {
        AdjustAfterSubtraction.ascii(AL).encodeByte should be(Hex.lsb("3F"))
      }
      "correctly represent aas as a string" in {
        AdjustAfterSubtraction.ascii(AL).toString should be("aas")
      }

      "correctly encode in das" in {
        AdjustAfterSubtraction.decimal(AL).encodeByte should be(Hex.lsb("2F"))
      }
      "correctly represent das as a string" in {
        AdjustAfterSubtraction.decimal(AL).toString should be("das")
      }
    }

    "in protected mode" should {
      import ProcessorMode.Protected._
      "correctly encode in aaa" in {
        AdjustAfterAddition.ascii(AL).encodeByte should be(Hex.lsb("37"))
      }
      "correctly represent aaa as a string" in {
        AdjustAfterAddition.ascii(AL).toString should be("aaa")
      }
    }
  }
}