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

class StackSuite extends AnyWordSpec with Matchers {

  "an Push instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode push WORD PTR [0x0001]" in {
        Push(MemoryAddress[WordSize](0x0001.toShort)).encodeByte should be(Hex.lsb("FF 36 01 00"))
      }

      "correctly encode push DWORD PTR [bx+si]" in {
        Push(RegisterMemoryLocation[DoubleWordSize](BX.combinedIndex(SI))).encodeByte should be(0x66.toByte :: 0xFF.toByte :: 0x30.toByte :: Nil)
      }

      "correctly encode push bx" in {
        Push(BX).encodeByte should be(0x53.toByte :: Nil)
      }

      "correctly encode push 0x01" in {
        Push(0x01.toByte).encodeByte should be(0x6A.toByte :: 0x01.toByte :: Nil)
      }

      "correctly encode push 0x1980" in {
        Push(0x1980.toShort).encodeByte should be(0x68.toByte :: 0x80.toByte :: 0x19.toByte :: Nil)
      }

      "correctly encode push cs" in {
        Push(CS).encodeByte should be(0x0E.toByte :: Nil)
      }
    }

    "in protected mode" should {

      import ProcessorMode.Protected._

      "correctly encode push eax" in {
        Push(EAX).encodeByte should be(0x50.toByte :: Nil)
      }

      "correctly encode push bx" in {
        Push(BX).encodeByte should be(0x66.toByte :: 0x53.toByte :: Nil)
      }

      "correctly encode push ss" in {
        Push(SS).encodeByte should be(0x16.toByte :: Nil)
      }

      "correctly encode push ds" in {
        Push(DS).encodeByte should be(0x1E.toByte :: Nil)
      }

      "correctly encode push es" in {
        Push(ES).encodeByte should be(0x06.toByte :: Nil)
      }

      "correctly encode push fs" in {
        Push(FS).encodeByte should be(0x0F.toByte :: 0xA0.toByte :: Nil)
      }

      "correctly encode push gs" in {
        Push(GS).encodeByte should be(0x0F.toByte :: 0xA8.toByte :: Nil)
      }
    }

    "in long mode" should {

      import ProcessorMode.Long._

      "correctly encode push QWORD PTR [rax]" in {
        Push(RegisterMemoryLocation[QuadWordSize](RAX)).encodeByte should be(0x48.toByte :: 0xFF.toByte :: 0x30.toByte :: Nil)
      }

      "correctly encode push bx" in {
        Push(BX).encodeByte should be(0x66.toByte :: 0x53.toByte :: Nil)
      }

      "correctly encode push r13" in {
        Push(R13).encodeByte should be(0x41.toByte :: 0x55.toByte :: Nil)
      }

      "correctly encode push rbp" in {
        Push(RBP).encodeByte should be(0x55.toByte :: Nil)
      }

      "correctly encode push rsi" in {
        Push(RSI).encodeByte should be(0x56.toByte :: Nil)
      }
    }
  }

  "a PushAll instruction" when {

    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode pusha" in {
        PushAll().encodeByte should be(Hex.lsb("60"))
      }

      "correctly represent pusha as a string" in {
        PushAll().toString shouldBe "pusha"
      }
     }
  }

  "an PushFlags instruction" when {
    "in long mode" should {

      import ProcessorMode.Long._

      "correctly encode pushf" in {
        PushFlags().encodeByte should be(Hex.lsb("9C"))
      }
    }
  }
}