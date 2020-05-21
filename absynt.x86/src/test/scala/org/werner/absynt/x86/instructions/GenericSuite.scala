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

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.werner.absynt.Hex
import org.werner.absynt.x86.ProcessorMode

class GenericSuite extends AnyWordSpec with Matchers {

  "a lock instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode in lock" in {
        Lock().encodeByte should be(Hex.lsb("F0"))
      }
      "correctly represent lock as a string" in {
        Lock().toString should be("lock")
      }
    }
  }

  "a hlt instruction" when {
    "in protected mode" should {

      import ProcessorMode.Protected._

      "correctly encode in hlt" in {
        Halt().encodeByte should be(Hex.lsb("F4"))
      }
      "correctly represent hlt as a string" in {
        Halt().toString should be("hlt")
      }
    }
  }

  "a cmc instruction" when {
    "in long mode" should {

      import ProcessorMode.Long._

      "correctly encode in cmc" in {
        ComplementCarry().encodeByte should be(Hex.lsb("F5"))
      }
      "correctly represent cmc as a string" in {
        ComplementCarry().toString should be("cmc")
      }
    }
  }
}