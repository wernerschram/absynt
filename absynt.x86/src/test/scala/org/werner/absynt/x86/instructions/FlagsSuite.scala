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

import org.werner.absynt.Hex
import org.werner.absynt.x86.ProcessorMode
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class FlagsSuite extends AnyWordSpec with Matchers {
  "an Increment instruction" when {
    "in real mode" should {

      import ProcessorMode.Real.{given, *}

      "correctly encode stc" in {
        SetCarryFlag().encodeByte should be(Hex.lsb("F9"))
      }
      "correctly represent stc as a string" in {
        SetCarryFlag().toString should be("stc")
      }

      "correctly encode std" in {
        SetDirectionFlag().encodeByte should be(Hex.lsb("FD"))
      }
      "correctly represent std as a string" in {
        SetDirectionFlag().toString should be("std")
      }

      "correctly encode sti" in {
        SetInterruptFlag().encodeByte should be(Hex.lsb("FB"))
      }
      "correctly represent sti as a string" in {
        SetInterruptFlag().toString should be("sti")
      }

      "correctly encode clc" in {
        ClearCarryFlag().encodeByte should be(Hex.lsb("F8"))
      }
      "correctly represent clc as a string" in {
        ClearCarryFlag().toString should be("clc")
      }

      "correctly encode cld" in {
        ClearDirectionFlag().encodeByte should be(Hex.lsb("FC"))
      }
      "correctly represent cld as a string" in {
        ClearDirectionFlag().toString should be("cld")
      }

      "correctly encode cli" in {
        ClearInterruptFlag().encodeByte should be(Hex.lsb("FA"))
      }
      "correctly represent cli as a string" in {
        ClearInterruptFlag().toString should be("cli")
      }
    }
  }
}
