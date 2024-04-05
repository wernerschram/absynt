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
import org.werner.absynt.x86.operands.WordSize

class ExchangeSuite extends AnyWordSpec with Matchers {

  "a Exchange instruction" when {
    "in real mode" should {

      import ProcessorMode.Real.{given, *}

      "correctly encode xchg bx, ax" in {
        Exchange(AX, BX).encodeByte should be(Hex.lsb("93"))
      }

      "correctly represent xchg bx, ax as a string" in {
        Exchange(AX, BX).toString should be("xchg bx, ax")
      }

      "correctly encode xchg ebx, eax" in {
        Exchange(EAX, EBX).encodeByte should be(Hex.lsb("66 93"))
      }

      "correctly represent xchg ebx, eax as a string" in {
        Exchange(EAX, EBX).toString should be("xchg ebx, eax")
      }


      "correctly encode xchg ax, bx" in {
        Exchange(BX, AX).encodeByte should be(Hex.lsb("93"))
      }

      "correctly represent xchg ax, bx as a string" in {
        Exchange(BX, AX).toString should be("xchg ax, bx")
      }

      "correctly encode xchg WORD PTR [si], si" in {
        Exchange(SI, RegisterMemoryLocation[WordSize](SI)).encodeByte should be(Hex.lsb("86 34"))
      }

      "correctly represent xchg WORD PTR [si], si as a string" in {
        Exchange(SI, RegisterMemoryLocation[WordSize](SI)).toString should be("xchg WORD PTR [si], si")
      }

      "correctly encode xchg WORD si, PTR [si]" in {
        Exchange(RegisterMemoryLocation[WordSize](SI), SI).encodeByte should be(Hex.lsb("86 34"))
      }

      "correctly represent xchg si, WORD PTR [si] as a string" in {
        Exchange(RegisterMemoryLocation[WordSize](SI), SI).toString should be("xchg si, WORD PTR [si]")
      }
    }

    "in protected mode" should {

      import ProcessorMode.Protected.{given, *}
      "correctly encode xchg bx, ax" in {
        Exchange(AX, BX).encodeByte should be(Hex.lsb("66 93"))
      }

      "correctly represent xchg bx, ax as a string" in {
        Exchange(AX, BX).toString should be("xchg bx, ax")
      }

      "correctly encode xchg ebx, eax" in {
        Exchange(EAX, EBX).encodeByte should be(Hex.lsb("93"))
      }

      "correctly represent xchg ebx, eax as a string" in {
        Exchange(EAX, EBX).toString should be("xchg ebx, eax")
      }


      "correctly encode xchg ax, bx" in {
        Exchange(BX, AX).encodeByte should be(Hex.lsb("66 93"))
      }

      "correctly represent xchg ax, bx as a string" in {
        Exchange(BX, AX).toString should be("xchg ax, bx")
      }

      "correctly encode xchg WORD PTR [si], si" in {
        Exchange(SI, RegisterMemoryLocation[WordSize](SI)).encodeByte should be(Hex.lsb("67 66 86 34"))
      }

      "correctly represent xchg WORD PTR [si], si as a string" in {
        Exchange(SI, RegisterMemoryLocation[WordSize](SI)).toString should be("xchg WORD PTR [si], si")
      }

      "correctly encode xchg WORD si, PTR [si]" in {
        Exchange(RegisterMemoryLocation[WordSize](SI), SI).encodeByte should be(Hex.lsb("67 66 86 34"))
      }

      "correctly represent xchg si, WORD PTR [si] as a string" in {
        Exchange(RegisterMemoryLocation[WordSize](SI), SI).toString should be("xchg si, WORD PTR [si]")
      }

    }

    "in long mode" should {

      import ProcessorMode.Long.{given, *}
      "correctly encode xchg bx, ax" in {
        Exchange(AX, BX).encodeByte should be(Hex.lsb("66 93"))
      }

      "correctly represent xchg bx, ax as a string" in {
        Exchange(AX, BX).toString should be("xchg bx, ax")
      }

      "correctly encode xchg ebx, eax" in {
        Exchange(EAX, EBX).encodeByte should be(Hex.lsb("93"))
      }

      "correctly represent xchg ebx, eax as a string" in {
        Exchange(EAX, EBX).toString should be("xchg ebx, eax")
      }


      "correctly encode xchg ax, bx" in {
        Exchange(BX, AX).encodeByte should be(Hex.lsb("66 93"))
      }

      "correctly represent xchg ax, bx as a string" in {
        Exchange(BX, AX).toString should be("xchg ax, bx")
      }

      "correctly encode xchg WORD PTR [si], si" in {
        Exchange(SI, RegisterMemoryLocation[WordSize](ESI)).encodeByte should be(Hex.lsb("67 66 86 36"))
      }

      "correctly represent xchg WORD PTR [si], si as a string" in {
        Exchange(SI, RegisterMemoryLocation[WordSize](ESI)).toString should be("xchg WORD PTR [esi], si")
      }

      "correctly encode xchg WORD si, PTR [si]" in {
        Exchange(RegisterMemoryLocation[WordSize](ESI), SI).encodeByte should be(Hex.lsb("67 66 86 36"))
      }

      "correctly represent xchg si, WORD PTR [si] as a string" in {
        Exchange(RegisterMemoryLocation[WordSize](ESI), SI).toString should be("xchg si, WORD PTR [esi]")
      }
    }
  }
}