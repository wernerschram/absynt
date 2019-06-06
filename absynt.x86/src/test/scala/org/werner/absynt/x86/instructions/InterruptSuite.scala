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
import org.werner.absynt.x86.operands.ImmediateValue._
import org.scalatest.{Matchers, WordSpec}

class InterruptSuite extends WordSpec with Matchers {

  "an Interrupt instruction" when {

    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode int 0x03" in {
        Interrupt(0x03.toByte).encodeByte should be (Hex.lsb("CC"))
      }

      "correctly represent int 0x03 as a string" in {
        Interrupt(0x03.toByte).toString should be("int 3")
      }
    }
    "in long mode" should {

      import ProcessorMode.Long._

      "correctly encode int 0x01" in {
        Interrupt(0x01.toByte).encodeByte should be (Hex.lsb("CD 01"))
      }

      "correctly represent int 0x01 as a string" in {
        Interrupt(0x01.toByte).toString should be("int 1")
      }
    }
  }
}