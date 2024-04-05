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
import org.werner.absynt.x86.operands.memoryaccess._
import org.werner.absynt.x86.operands.{DoubleWordSize, QuadWordSize, WordSize}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scala.language.implicitConversions

class StackSuite extends AnyWordSpec with Matchers {

  "a Push instruction" when {
    "in real mode" should {

      import ProcessorMode.Real.{given, *}

      "correctly encode push WORD PTR [0x0001]" in {
        Push(MemoryAddress[WordSize](0x0001.toShort)).encodeByte should be(Hex.lsb("FF 36 01 00"))
      }

      "correctly encode push DWORD PTR [bx+si]" in {
        Push(RegisterMemoryLocation[DoubleWordSize](BX+SI)).encodeByte should be(Hex.lsb("66 FF 30"))
      }

      "correctly encode push bx" in {
        Push(BX).encodeByte should be(Hex.lsb("53"))
      }

      "correctly encode push 0x01" in {
        Push(0x01.toByte).encodeByte should be(Hex.lsb("6A 01"))
      }

      "correctly encode push 0x1980" in {
        Push(0x1980.toShort).encodeByte should be(Hex.lsb("68 80 19"))
      }

      "correctly encode push cs" in {
        Push(CS).encodeByte should be(Hex.lsb("0E"))
      }
    }

    "in protected mode" should {

      import ProcessorMode.Protected.{given, *}

      "correctly encode push eax" in {
        Push(EAX).encodeByte should be(Hex.lsb("50"))
      }

      "correctly encode push bx" in {
        Push(BX).encodeByte should be(Hex.lsb("66 53"))
      }
      "correctly represent push bx as a string" in {
        Push(BX).toString should be("push bx")
      }

      "correctly encode push ss" in {
        Push(SS).encodeByte should be(Hex.lsb("16"))
      }
      "correctly represent push ss as a string" in {
        Push(SS).toString should be("push ss")
      }

      "correctly encode push ds" in {
        Push(DS).encodeByte should be(Hex.lsb("1E"))
      }
      "correctly represent push ds as a string" in {
        Push(DS).toString should be("push ds")
      }

      "correctly encode push es" in {
        Push(ES).encodeByte should be(Hex.lsb("06"))
      }
      "correctly represent push es as a string" in {
        Push(ES).toString should be("push es")
      }

      "correctly encode push fs" in {
        Push(FS).encodeByte should be(Hex.lsb("0F A0"))
      }
      "correctly represent push fs as a string" in {
        Push(FS).toString should be("push fs")
      }

      "correctly encode push gs" in {
        Push(GS).encodeByte should be(Hex.lsb("0F A8"))
      }
      "correctly represent push gs as a string" in {
        Push(GS).toString should be("push gs")
      }

      "correctly encode pushw ss" in {
        Push.Unaligned(SS).encodeByte should be(Hex.lsb("66 16"))
      }
      "correctly represent pushw ss as a string" in {
        Push.Unaligned(SS).toString should be("pushw ss")
      }

      "correctly encode pushw ds" in {
        Push.Unaligned(DS).encodeByte should be(Hex.lsb("66 1E"))
      }
      "correctly represent pushw ds as a string" in {
        Push.Unaligned(DS).toString should be("pushw ds")
      }

      "correctly encode pushw es" in {
        Push.Unaligned(ES).encodeByte should be(Hex.lsb("66 06"))
      }
      "correctly represent pushw es as a string" in {
        Push.Unaligned(ES).toString should be("pushw es")
      }

      "correctly encode pushw fs" in {
        Push.Unaligned(FS).encodeByte should be(Hex.lsb("66 0F A0"))
      }
      "correctly represent pushw fs as a string" in {
        Push.Unaligned(FS).toString should be("pushw fs")
      }

      "correctly encode pushw gs" in {
        Push.Unaligned(GS).encodeByte should be(Hex.lsb("66 0F A8"))
      }
      "correctly represent pushw gs as a string" in {
        Push.Unaligned(GS).toString should be("pushw gs")
      }
    }

    "in long mode" should {

      import ProcessorMode.Long.{given, *}

      "correctly encode push QWORD PTR [rax]" in {
        Push(RegisterMemoryLocation[QuadWordSize](RAX)).encodeByte should be(Hex.lsb("48 FF 30"))
      }

      "correctly encode push bx" in {
        Push(BX).encodeByte should be(Hex.lsb("66 53"))
      }

      "correctly encode push r13" in {
        Push(R13).encodeByte should be(Hex.lsb("41 55"))
      }

      "correctly encode push rbp" in {
        Push(RBP).encodeByte should be(Hex.lsb("55"))
      }

      "correctly encode push rsi" in {
        Push(RSI).encodeByte should be(Hex.lsb("56"))
      }
    }
  }

  "a PushAll instruction" when {

    "in real mode" should {

      import ProcessorMode.Real.{given, *}

      "correctly encode pusha" in {
        PushAll().encodeByte should be(Hex.lsb("60"))
      }

      "correctly represent pusha as a string" in {
        PushAll().toString shouldBe "pusha"
      }
     }
  }

  "a PushFlags instruction" when {
    "in long mode" should {

      import ProcessorMode.Long.{given, *}

      "correctly encode pushf" in {
        PushFlags().encodeByte should be(Hex.lsb("9C"))
      }
    }
  }

  "a Pop instruction" when {
    "in real mode" should {

      import ProcessorMode.Real.{given, *}

      "correctly encode pop WORD PTR [0x0001]" in {
        Pop(MemoryAddress[WordSize](0x0001.toShort)).encodeByte should be(Hex.lsb("8F 36 01 00"))
      }
      "correctly represent pop WORD PTR [0x0001] as a string" in {
        Pop(MemoryAddress[WordSize](0x0001.toShort)).toString should be("pop WORD PTR [1]")
      }

      "correctly encode pop DWORD PTR [bx+si]" in {
        Pop(RegisterMemoryLocation[DoubleWordSize](BX.combinedIndex(SI))).encodeByte should be(Hex.lsb("66 8F 30"))
      }
      "correctly represent pop DWORD PTR [bx+si] as a string" in {
        Pop(RegisterMemoryLocation[DoubleWordSize](BX+SI)).toString should be("pop DWORD PTR [bx+si]")
      }

      "correctly encode pop bx" in {
        Pop(BX).encodeByte should be(Hex.lsb("5B"))
      }
      "correctly represent pop bx as a string" in {
        Pop(BX).toString should be("pop bx")
      }

      "correctly encode pop ebx" in {
        Pop(EBX).encodeByte should be(Hex.lsb("66 5B"))
      }
      "correctly represent pop ebx as a string" in {
        Pop(EBX).toString should be("pop ebx")
      }

      "correctly encode pop ss" in {
        Pop(SS).encodeByte should be(Hex.lsb("17"))
      }
      "correctly represent pop ss as a string" in {
        Pop(SS).toString should be("pop ss")
      }

      "correctly encode pop ds" in {
        Pop(DS).encodeByte should be(Hex.lsb("1F"))
      }
      "correctly represent pop ds as a string" in {
        Pop(DS).toString should be("pop ds")
      }

      "correctly encode pop es" in {
        Pop(ES).encodeByte should be(Hex.lsb("07"))
      }
      "correctly represent pop es as a string" in {
        Pop(ES).toString should be("pop es")
      }

      "correctly encode pop fs" in {
        Pop(FS).encodeByte should be(Hex.lsb("0F A1"))
      }
      "correctly represent pop fs as a string" in {
        Pop(FS).toString should be("pop fs")
      }

      "correctly encode pop gs" in {
        Pop(GS).encodeByte should be(Hex.lsb("0F A9"))
      }
      "correctly represent pop gs as a string" in {
        Pop(GS).toString should be("pop gs")
      }

      "correctly encode popw ss" in {
        Pop.Unaligned(SS).encodeByte should be(Hex.lsb("66 17"))
      }
      "correctly represent popw ss as a string" in {
        Pop.Unaligned(SS).toString should be("popw ss")
      }

      "correctly encode popw ds" in {
        Pop.Unaligned(DS).encodeByte should be(Hex.lsb("66 1F"))
      }
      "correctly represent popw ds as a string" in {
        Pop.Unaligned(DS).toString should be("popw ds")
      }

      "correctly encode popw es" in {
        Pop.Unaligned(ES).encodeByte should be(Hex.lsb("66 07"))
      }
      "correctly represent popw es as a string" in {
        Pop.Unaligned(ES).toString should be("popw es")
      }

      "correctly encode popw fs" in {
        Pop.Unaligned(FS).encodeByte should be(Hex.lsb("66 0F A1"))
      }
      "correctly represent popw fs as a string" in {
        Pop.Unaligned(FS).toString should be("popw fs")
      }

      "correctly encode popw gs" in {
        Pop.Unaligned(GS).encodeByte should be(Hex.lsb("66 0F A9"))
      }
      "correctly represent popw gs as a string" in {
        Pop.Unaligned(GS).toString should be("popw gs")
      }
    }
  }

  "a PopAll instruction" when {

    "in real mode" should {

      import ProcessorMode.Real.{given, *}

      "correctly encode popa" in {
        PopAll().encodeByte should be(Hex.lsb("61"))
      }

      "correctly represent popa as a string" in {
        PopAll().toString shouldBe "popa"
      }
    }
  }

  "a PopFlags instruction" when {
    "in long mode" should {

      import ProcessorMode.Long.{given, *}

      "correctly encode popf" in {
        PopFlags().encodeByte should be(Hex.lsb("9D"))
      }
    }
  }
}