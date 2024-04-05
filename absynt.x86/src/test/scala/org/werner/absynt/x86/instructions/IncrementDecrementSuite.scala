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
import org.werner.absynt.x86.operands.{ByteSize, WordSize}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class IncrementDecrementSuite extends AnyWordSpec with Matchers {

  "an Increment instruction" when {
    "in real mode" should {

      import ProcessorMode.Real.{given, *}

      "correctly encode inc al" in {
        Increment(AL).encodeByte should be(Hex.lsb("FE C0"))
      }
      "correctly represent inc al as a string" in {
        Increment(AL).toString should be("inc al")
      }

      "correctly encode inc BYTE PTR [bp+di]" in {
        Increment(RegisterMemoryLocation[ByteSize](BP+DI)).encodeByte should be(Hex.lsb("FE 03"))
      }
      "correctly represent inc BYTE PTR [bp+di] as a string" in {
        Increment(RegisterMemoryLocation[ByteSize](BP+DI)).toString should be("inc BYTE PTR [bp+di]")
      }

      "correctly encode inc ax" in {
        Increment(AX).encodeByte should be(Hex.lsb("40"))
      }
      "correctly represent inc ax as a string" in {
        Increment(AX).toString should be("inc ax")
      }

      "correctly encode inc WORD PTR [bp+di]" in {
        Increment(RegisterMemoryLocation[WordSize](BP+DI)).encodeByte should be(Hex.lsb("FF 03"))
      }
      "correctly represent inc WORD PTR [bp+di] as a string" in {
        Increment(RegisterMemoryLocation[WordSize](BP+DI)).toString should be("inc WORD PTR [bp+di]")
      }

      "correctly encode inc esp" in {
        Increment(ESP).encodeByte should be(Hex.lsb("66 44"))
      }
      "correctly represent inc esp as a string" in {
        Increment(ESP).toString should be("inc esp")
      }
    }

    "in protected mode" should {
      import ProcessorMode.Protected.{given, *}

      "correctly encode inc bl" in {
        Increment(BL).encodeByte should be(Hex.lsb("FE C3"))
      }
      "correctly represent inc bl as a string" in {
        Increment(BL).toString should be("inc bl")
      }

      "correctly encode inc ax" in {
        Increment(AX).encodeByte should be(Hex.lsb("66 40"))
      }
      "correctly represent inc ax as a string" in {
        Increment(AX).toString should be("inc ax")
      }

      "correctly encode inc ebp" in {
        Increment(EBP).encodeByte should be(Hex.lsb("45"))
      }
      "correctly represent inc ebp as a string" in {
        Increment(EBP).toString should be("inc ebp")
      }
    }
    "in long mode" should {

      import ProcessorMode.Long.{given, *}

      "correctly encode inc r8l" in {
        Increment(R8L).encodeByte should be(Hex.lsb("41 FE C0"))
      }
      "correctly represent inc r8l as a string" in {
        Increment(R8L).toString should be("inc r8l")
      }

      "correctly encode inc ax" in {
        Increment(AX).encodeByte should be(Hex.lsb("66 FF C0"))
      }
      "correctly represent inc ax as a string" in {
        Increment(AX).toString should be("inc ax")
      }

      "correctly encode inc esi" in {
        Increment(ESI).encodeByte should be(Hex.lsb("FF C6"))
      }
      "correctly represent inc esp as a string" in {
        Increment(ESI).toString should be("inc esi")
      }

      "correctly encode inc r10l" in {
        Increment(R10).encodeByte should be(Hex.lsb("49 FF C2"))
      }
      "correctly represent inc r10l as a string" in {
        Increment(R10).toString should be("inc r10")
      }
    }
  }
}