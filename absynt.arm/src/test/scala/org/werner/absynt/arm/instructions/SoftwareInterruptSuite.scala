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
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SoftwareInterruptSuite extends AnyWordSpec with Matchers {

  "a SoftwareInterrupt instruction" when {
    "in a32 mode" should {

      import ProcessorMode.A32._

      "correctly encode swi 10" in {
        SoftwareInterrupt(10).encodeByte should be(Hex.msb("ef00000a"))
      }

      "correctly represent swi 10 as a string" in {
        SoftwareInterrupt(10).toString should be("swi 10")
      }
    }
  }
}