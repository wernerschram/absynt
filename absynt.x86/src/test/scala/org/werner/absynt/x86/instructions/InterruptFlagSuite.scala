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

class InterruptFlagSuite extends AnyWordSpec with Matchers {

  "a ClearInterruptFlag instruction" when {

    "in real mode" should {

      import ProcessorMode.Real.{given, *}

      "correctly encode cli" in {
        ClearInterruptFlag().encodeByte should be(Hex.lsb("FA"))
      }
    }

    "in long mode" should {

      import ProcessorMode.Long.{given, *}

      "correctly encode cli" in {
        ClearInterruptFlag().encodeByte should be(Hex.lsb("FA"))
      }
    }
  }

  "a SetInterruptFlag instruction" when {

    "in real mode" should {

      import ProcessorMode.Real.{given, *}

      "correctly encode sti" in {
        SetInterruptFlag().encodeByte should be(Hex.lsb("FB"))
      }
    }
    "in long mode" should {

      import ProcessorMode.Long.{given, *}

      "correctly encode sti" in {
        SetInterruptFlag().encodeByte should be(Hex.lsb("FB"))
      }
    }
  }
}
