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

class DivideSuite extends WordSpec with Matchers {

  "an Divide instruction" when {

    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode div bl" in {
        Divide(BL).encodeByte should be (Hex.lsb("F6 F3"))
      }
      "correctly represent div bl as a string" in {
        Divide(BL).toString should be("div bl")
      }

      "correctly encode div bx" in {
        Divide(BX).encodeByte should be (Hex.lsb("F7 F3"))
      }
      "correctly represent div bx as a string" in {
        Divide(BX).toString should be("div bx")
      }

      "correctly encode div ebx" in {
        Divide(EBX).encodeByte should be (Hex.lsb("66 F7 F3"))
      }
      "correctly represent div ebx as a string" in {
        Divide(EBX).toString should be("div ebx")
      }
    }

    "in protected mode" should {

      import ProcessorMode.Protected._

      "correctly encode div bl" in {
        Divide(BL).encodeByte should be(Hex.lsb("F6 F3"))
      }
      "correctly represent div bl as a string" in {
        Divide(BL).toString should be("div bl")
      }

      "correctly encode div bx" in {
        Divide(BX).encodeByte should be(Hex.lsb("66 F7 F3"))
      }
      "correctly represent div bx as a string" in {
        Divide(BX).toString should be("div bx")
      }

      "correctly encode div ebx" in {
        Divide(EBX).encodeByte should be(Hex.lsb("F7 F3"))
      }
      "correctly represent div ebx as a string" in {
        Divide(EBX).toString should be("div ebx")
      }
    }

    "in long mode" should {

      import ProcessorMode.Long._

      "correctly encode div bl" in {
        Divide(BL).encodeByte should be(Hex.lsb("F6 F3"))
      }
      "correctly represent div bl as a string" in {
        Divide(BL).toString should be("div bl")
      }

      "correctly encode div r8l" in {
        Divide(R8L).encodeByte should be(Hex.lsb("41 F6 F0"))
      }
      "correctly represent div r8l as a string" in {
        Divide(R8L).toString should be("div r8l")
      }

      "correctly encode div bx" in {
        Divide(BX).encodeByte should be(Hex.lsb("66 F7 F3"))
      }
      "correctly represent div bx as a string" in {
        Divide(BX).toString should be("div bx")
      }

      "correctly encode div ebx" in {
        Divide(EBX).encodeByte should be(Hex.lsb("F7 F3"))
      }
      "correctly represent div ebx as a string" in {
        Divide(EBX).toString should be("div ebx")
      }

      "correctly encode div rbx" in {
        Divide(RBX).encodeByte should be(Hex.lsb("48 F7 F3"))
      }
      "correctly represent div rbx as a string" in {
        Divide(RBX).toString should be("div rbx")
      }
    }
  }
}