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
import org.werner.absynt.x86.operands.ByteSize
import org.werner.absynt.x86.operands.memoryaccess._
import scala.language.implicitConversions

class ShiftSuite extends AnyWordSpec with Matchers {

  "a ShiftArithmeticLeft instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode sal BYTE PTR [0x0001], 1" in {
        ShiftArithmeticLeft(1.toByte, MemoryAddress[ByteSize](0x0001.toShort)).encodeByte should be(Hex.lsb("D0 26 01 00"))
      }

      "correctly represent sal BYTE PTR [0x0001], 1 as a string" in {
        ShiftArithmeticLeft(1.toByte, MemoryAddress[ByteSize](0x0001.toShort)).toString should be("sal BYTE PTR [1], 1")
      }

      "correctly encode sal al, 2" in {
        ShiftArithmeticLeft(2.toByte, AL).encodeByte should be(Hex.lsb("C0 E0 02"))
      }

      "correctly represent sal al, 2 as a string" in {
        ShiftArithmeticLeft(2.toByte, AL).toString should be("sal al, 2")
      }

      "correctly encode sal cl, cl" in {
        ShiftArithmeticLeft(CL, CL).encodeByte should be(Hex.lsb("D2 E1"))
      }

      "correctly represent sal cl, cl as a string" in {
        ShiftArithmeticLeft(CL, CL).toString should be("sal cl, cl")
      }

      "correctly encode sal si, cl" in {
        ShiftArithmeticLeft(CL, ESI).encodeByte should be(Hex.lsb("66 D3 E6"))
      }

      "correctly represent sal si, cl as a string" in {
        ShiftArithmeticLeft(CL, ESI).toString should be("sal esi, cl")
      }

    }

    "in protected mode" should {

      import ProcessorMode.Protected._

      "correctly encode sal BYTE PTR [0x0001], 1" in {
        ShiftArithmeticLeft(1.toByte, MemoryAddress[ByteSize](0x0001.toShort)).encodeByte should be(Hex.lsb("67 D0 26 01 00"))
      }

      "correctly represent sal BYTE PTR [0x0001], 1 as a string" in {
        ShiftArithmeticLeft(1.toByte, MemoryAddress[ByteSize](0x0001.toShort)).toString should be("sal BYTE PTR [1], 1")
      }

      "correctly encode sal al, 2" in {
        ShiftArithmeticLeft(2.toByte, AL).encodeByte should be(Hex.lsb("C0 E0 02"))
      }

      "correctly represent sal al, 2 as a string" in {
        ShiftArithmeticLeft(2.toByte, AL).toString should be("sal al, 2")
      }

      "correctly encode sal cl, cl" in {
        ShiftArithmeticLeft(CL, CL).encodeByte should be(Hex.lsb("D2 E1"))
      }

      "correctly represent sal cl, cl as a string" in {
        ShiftArithmeticLeft(CL, CL).toString should be("sal cl, cl")
      }

      "correctly encode sal si, cl" in {
        ShiftArithmeticLeft(CL, ESI).encodeByte should be(Hex.lsb("D3 E6"))
      }

      "correctly represent sal si, cl as a string" in {
        ShiftArithmeticLeft(CL, ESI).toString should be("sal esi, cl")
      }

    }

    "in long mode" should {

      import ProcessorMode.Long._
      "correctly encode sal BYTE PTR [0x0001], 1" in {
        //TODO: this is not allowed in long mode
        ShiftArithmeticLeft(1.toByte, MemoryAddress[ByteSize](0x0001.toShort)).encodeByte should be(Hex.lsb("D0 26 01 00"))
      }

      "correctly represent sal BYTE PTR [0x0001], 1 as a string" in {
        ShiftArithmeticLeft(1.toByte, MemoryAddress[ByteSize](0x0001.toShort)).toString should be("sal BYTE PTR [1], 1")
      }

      "correctly encode sal al, 2" in {
        ShiftArithmeticLeft(2.toByte, AL).encodeByte should be(Hex.lsb("C0 E0 02"))
      }

      "correctly represent sal al, 2 as a string" in {
        ShiftArithmeticLeft(2.toByte, AL).toString should be("sal al, 2")
      }

      "correctly encode sal cl, cl" in {
        ShiftArithmeticLeft(CL, CL).encodeByte should be(Hex.lsb("D2 E1"))
      }

      "correctly represent sal cl, cl as a string" in {
        ShiftArithmeticLeft(CL, CL).toString should be("sal cl, cl")
      }

      "correctly encode sal si, cl" in {
        ShiftArithmeticLeft(CL, RSI).encodeByte should be(Hex.lsb("48 D3 E6"))
      }

      "correctly represent sal si, cl as a string" in {
        ShiftArithmeticLeft(CL, RSI).toString should be("sal rsi, cl")
      }

    }
  }
  "a ShiftArithmeticRight instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode sar BYTE PTR [0x0001], 1" in {
        ShiftArithmeticRight(1.toByte, MemoryAddress[ByteSize](0x0001.toShort)).encodeByte should be(Hex.lsb("D0 3E 01 00"))
      }

      "correctly represent sar BYTE PTR [0x0001], 1 as a string" in {
        ShiftArithmeticRight(1.toByte, MemoryAddress[ByteSize](0x0001.toShort)).toString should be("sar BYTE PTR [1], 1")
      }
    }
  }

  "a ShiftLogicalLeft instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode shl BYTE PTR [0x0001], 1" in {
        ShiftLogicalLeft(1.toByte, MemoryAddress[ByteSize](0x0001.toShort)).encodeByte should be(Hex.lsb("D0 26 01 00"))
      }

      "correctly represent shl BYTE PTR [0x0001], 1 as a string" in {
        ShiftLogicalLeft(1.toByte, MemoryAddress[ByteSize](0x0001.toShort)).toString should be("shl BYTE PTR [1], 1")
      }
    }
  }

  "a ShiftLogicalRight instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode shr BYTE PTR [0x0001], 1" in {
        ShiftLogicalRight(1.toByte, MemoryAddress[ByteSize](0x0001.toShort)).encodeByte should be(Hex.lsb("D0 2E 01 00"))
      }

      "correctly represent shr BYTE PTR [0x0001], 1 as a string" in {
        ShiftLogicalRight(1.toByte, MemoryAddress[ByteSize](0x0001.toShort)).toString should be("shr BYTE PTR [1], 1")
      }
    }
  }

  "a RotateCarryLeft instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode rcl BYTE PTR [0x0001], 1" in {
        RotateCarryLeft(1.toByte, MemoryAddress[ByteSize](0x0001.toShort)).encodeByte should be(Hex.lsb("D0 16 01 00"))
      }

      "correctly represent rcl BYTE PTR [0x0001], 1 as a string" in {
        RotateCarryLeft(1.toByte, MemoryAddress[ByteSize](0x0001.toShort)).toString should be("rcl BYTE PTR [1], 1")
      }
    }
  }

  "a RotateCarryRight instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode rcr BYTE PTR [0x0001], 1" in {
        RotateCarryRight(1.toByte, MemoryAddress[ByteSize](0x0001.toShort)).encodeByte should be(Hex.lsb("D0 1E 01 00"))
      }

      "correctly represent rcr BYTE PTR [0x0001], 1 as a string" in {
        RotateCarryRight(1.toByte, MemoryAddress[ByteSize](0x0001.toShort)).toString should be("rcr BYTE PTR [1], 1")
      }
    }
  }

  "a RotateLeft instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode rol BYTE PTR [0x0001], 1" in {
        RotateLeft(1.toByte, MemoryAddress[ByteSize](0x0001.toShort)).encodeByte should be(Hex.lsb("D0 06 01 00"))
      }

      "correctly represent rol BYTE PTR [0x0001], 1 as a string" in {
        RotateLeft(1.toByte, MemoryAddress[ByteSize](0x0001.toShort)).toString should be("rol BYTE PTR [1], 1")
      }
    }
  }

  "a RotateRight instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode ror BYTE PTR [0x0001], 1" in {
        RotateRight(1.toByte, MemoryAddress[ByteSize](0x0001.toShort)).encodeByte should be(Hex.lsb("D0 0E 01 00"))
      }

      "correctly represent ror BYTE PTR [0x0001], 1 as a string" in {
        RotateRight(1.toByte, MemoryAddress[ByteSize](0x0001.toShort)).toString should be("ror BYTE PTR [1], 1")
      }
    }
  }
}