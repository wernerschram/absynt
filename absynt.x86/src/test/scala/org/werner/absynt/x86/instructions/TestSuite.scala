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
import org.werner.absynt.x86.operands.{ByteSize, DoubleWordSize, QuadWordSize, WordSize}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scala.language.implicitConversions

class TestSuite extends AnyWordSpec with Matchers {

  "a Test instruction" when {

    "in legacy mode" should {
      import ProcessorMode.Legacy.{given, *}

      "correctly encode test bl, 0x40" in {
        Test(0x40.toByte, BL).encodeByte should be(Hex.lsb("F6 C3 40"))
      }
    }

    "in real mode" should {

      import ProcessorMode.Real.{given, *}

      "correctly encode test al, 0x40" in {
        Test(0x40.toByte, AL).encodeByte should be(Hex.lsb("A8 40"))
      }

      "correctly represent test al, 0x40 as a string" in {
        Test(0x40.toByte, AL).toString shouldBe "test al, 64"
      }

      "correctly encode test ax, 0x3412" in {
        Test(0x3412.toShort, AX).encodeByte should be(Hex.lsb("A9 12 34"))
      }

      "correctly represent test ax, 0x3412 as a string" in {
        Test(0x3412.toShort, AX).toString shouldBe "test ax, 13330"
      }

      "correctly encode test al, al" in {
        Test(AL, AL).encodeByte should be(Hex.lsb("84 C0"))
      }

      "correctly represent test al, al as a string" in {
        Test(AL, AL).toString shouldBe "test al, al"
      }

      "correctly encode test ax, ax" in {
        Test(AX, AX).encodeByte should be(Hex.lsb("85 C0"))
      }

      "correctly represent test ax, ax as a string" in {
        Test(AX, AX).toString shouldBe "test ax, ax"
      }

      "correctly encode test eax, 0x44332211" in {
        Test(0x44332211, EAX).encodeByte should be(Hex.lsb("66 A9 11 22 33 44"))
      }

      "correctly represent test ax, 0x44332211 as a string" in {
        Test(0x44332211, EAX).toString shouldBe "test eax, 1144201745"
      }

      "correctly encode test bl, 0x40" in {
        Test(0x40.toByte, BL).encodeByte should be(Hex.lsb("F6 C3 40"))
      }

      "correctly represent test bl, 0x40 as a string" in {
        Test(0x40.toByte, BL).toString shouldBe "test bl, 64"
      }

      "correctly encode test WORD PTR [bx], 0x3412" in {
        Test(0x3412.toShort, RegisterMemoryLocation[WordSize](BX)).encodeByte should be(Hex.lsb("F7 07 12 34"))
      }

      "correctly represent test WORD PTR [bx], 0x3412 as a string" in {
        Test(0x3412.toShort, RegisterMemoryLocation[WordSize](BX)).toString shouldBe "test WORD PTR [bx], 13330"
      }

      "correctly encode test BYTE PTR [bx], al" in {
        Test(AL, RegisterMemoryLocation[ByteSize](BX)).encodeByte should be(Hex.lsb("84 07"))
      }

      "correctly represent test BYTE PTR [bx], al as a string" in {
        Test(AL, RegisterMemoryLocation[ByteSize](BX)).toString shouldBe "test BYTE PTR [bx], al"
      }
    }

    "in protected mode" should {

      import ProcessorMode.Protected.{given, *}

      "correctly encode test DWORD PTR [0x11111111], 0x44332211" in {
        Test(0x44332211, MemoryAddress[DoubleWordSize](0x11111111)).encodeByte should be(Hex.lsb("F7 05 11 11 11 11 11 22 33 44"))
      }

      "correctly represent test DWORD PTR [0x11111111], 0x44332211 as a string" in {
        Test(0x44332211, MemoryAddress[DoubleWordSize](0x11111111)).toString shouldBe "test DWORD PTR [286331153], 1144201745"
      }

      "correctly encode test DWORD PTR [0x1234], edx" in {
        Test(EDX, MemoryAddress[DoubleWordSize](0X1234.toShort)).encodeByte should be(Hex.lsb("67 85 16 34 12"))
      }

      "correctly represent test DWORD PTR [0x1234], edx as a string" in {
        Test(EDX, MemoryAddress[DoubleWordSize](0x1234.toShort)).toString shouldBe "test DWORD PTR [4660], edx"
      }
    }

    "in long mode" should {

      import ProcessorMode.Long.{given, *}

      "correctly encode test QWORD PTR [0x11111111], 0x44332211" in {
        Test(0x44332211, MemoryAddress[QuadWordSize](0x11111111)).encodeByte should be(Hex.lsb("67 48 F7 05 11 11 11 11 11 22 33 44"))
      }

      "correctly represent test QWORD PTR [0x11111111], 0x44332211 as a string" in {
        Test(0x44332211, MemoryAddress[QuadWordSize](0x11111111)).toString shouldBe "test QWORD PTR [286331153], 1144201745"
      }

      "throw an AssertionError for test WORD PTR [0x11111111], 0x44332211" in {
        an[AssertionError] should be thrownBy {
          Test(0x44332211, MemoryAddress[WordSize](0x11111111))
        }
      }

      "correctly encode test rax, 0x78776655" in {
        Test(0x78776655, RAX).encodeByte should be(Hex.lsb("48 A9 55 66 77 78"))
      }

      "correctly represent test rax, 0x78776655 as a string" in {
        Test(0x78776655, RAX).toString shouldBe "test rax, 2021090901"
      }

      // Note that the GNU assembler (and likely others) denotes this as test RAX, 0xFFFFFFFF88776655 and doens't accept this notation.
      "correctly encode test rax, 0x88776655" in {
        Test(0x88776655, RAX).encodeByte should be(Hex.lsb("48 A9 55 66 77 88"))
      }

      "correctly represent test rax, 0x88776655 as a string" in {
        Test(0x88776655, RAX).toString shouldBe "test rax, -2005440939"
      }

      "correctly encode test BYTE PTR [rax+rbx*2], 0x11" in {
        Test(0x11.toByte, SIBMemoryLocation[ByteSize](RBX, RAX, scale = 2)).encodeByte should be(Hex.lsb("F6 04 58 11"))
      }

      "correctly represent test BYTE PTR [rax+rbx*2], 0x11 as a string" in {
        Test(0x11.toByte, SIBMemoryLocation[ByteSize](RBX, RAX, scale = 2)).toString shouldBe "test BYTE PTR [rax+rbx*2], 17"
      }

      "correctly encode test WORD PTR [rax+rbx*2], 0x2211" in {
        Test(0x2211.toShort, SIBMemoryLocation[WordSize](RBX, RAX, scale = 2)).encodeByte should be(Hex.lsb("66 F7 04 58 11 22"))
      }

      "correctly represent test WORD PTR [rax+rbx*2], 0x2211 as a string" in {
        Test(0x2211.toShort, SIBMemoryLocation[WordSize](RBX, RAX, scale = 2)).toString shouldBe "test WORD PTR [rax+rbx*2], 8721"
      }

      "correctly encode test DWORD PTR [rax+rbx*2], 0x44332211" in {
        Test(0x44332211, SIBMemoryLocation[DoubleWordSize](RBX, RAX, scale = 2)).encodeByte should be(Hex.lsb("F7 04 58 11 22 33 44"))
      }

      "correctly represent test DWORD PTR [rax+rbx*2], 0x44332211 as a string" in {
        Test(0x44332211, SIBMemoryLocation[DoubleWordSize](RBX, RAX, scale = 2)).toString shouldBe "test DWORD PTR [rax+rbx*2], 1144201745"
      }

      "correctly encode test QWORD PTR [rax+rbx*2], 0x44332211" in {
        Test(0x44332211, SIBMemoryLocation[QuadWordSize](RBX, RAX, scale = 2)).encodeByte should be(Hex.lsb("48 F7 04 58 11 22 33 44"))
      }

      "correctly represent test QWORD PTR [rax+rbx*2], 0x44332211 as a string" in {
        Test(0x44332211, SIBMemoryLocation[QuadWordSize](RBX, RAX, scale = 2)).toString shouldBe "test QWORD PTR [rax+rbx*2], 1144201745"
      }

      "correctly encode test BYTE PTR gs:[rax+rbx*2], 0x11" in {
        Test(0x11.toByte, SIBMemoryLocation.withSegmentOverride[ByteSize](RBX, RAX, scale = 2, segment = GS)).encodeByte should be(Hex.lsb("65 F6 04 58 11"))
      }

      "correctly represent test BYTE PTR gs:[rax+rbx*2], 0x11 as a string" in {
        Test(0x11.toByte, SIBMemoryLocation.withSegmentOverride[ByteSize](RBX, RAX, scale = 2, segment = GS)).toString shouldBe "test BYTE PTR gs:[rax+rbx*2], 17"
      }

      "correctly encode test WORD PTR es:[rax+rbx*2], 0x2211" in {
        Test(0x2211.toShort, SIBMemoryLocation.withSegmentOverride[WordSize](RBX, RAX, scale = 2, segment = ES)).encodeByte should be(Hex.lsb("26 66 F7 04 58 11 22"))
      }

      "correctly represent test WORD PTR es:[rax+rbx*2], 0x2211 as a string" in {
        Test(0x2211.toShort, SIBMemoryLocation.withSegmentOverride[WordSize](RBX, RAX, scale = 2, segment = ES)).toString shouldBe "test WORD PTR es:[rax+rbx*2], 8721"
      }

      "correctly encode test DWORD PTR fs:[rax+rbx*2], 0x44332211" in {
        Test(0x44332211, SIBMemoryLocation.withSegmentOverride[DoubleWordSize](RBX, RAX, scale = 2, segment = FS)).encodeByte should be(Hex.lsb("64 F7 04 58 11 22 33 44"))
      }

      "correctly represent test DWORD PTR fs:[rax+rbx*2], 0x44332211 as a string" in {
        Test(0x44332211, SIBMemoryLocation.withSegmentOverride[DoubleWordSize](RBX, RAX, scale = 2, segment = FS)).toString shouldBe "test DWORD PTR fs:[rax+rbx*2], 1144201745"
      }

      "correctly encode test QWORD PTR ss:[rax+rbx*2], 0x44332211" in {
        Test(0x44332211, SIBMemoryLocation.withSegmentOverride[QuadWordSize](RBX, RAX, scale = 2, segment = SS)).encodeByte should be(Hex.lsb("36 48 F7 04 58 11 22 33 44"))
      }

      "correctly represent test QWORD PTR ss:[rax+rbx*2], 0x44332211 as a string" in {
        Test(0x44332211, SIBMemoryLocation.withSegmentOverride[QuadWordSize](RBX, RAX, scale = 2, segment = SS)).toString shouldBe "test QWORD PTR ss:[rax+rbx*2], 1144201745"
      }

      "correctly encode test QWORD PTR cs:[eax], 0x44332211" in {
        Test(0x44332211, RegisterMemoryLocation[QuadWordSize](CS/EAX)).encodeByte should be(Hex.lsb("2e 67 48 F7 00 11 22 33 44"))
      }

      "correctly represent test QWORD PTR cs:[eax], 0x44332211 as a string" in {
        Test(0x44332211, RegisterMemoryLocation[QuadWordSize](CS/EAX)).toString shouldBe "test QWORD PTR cs:[eax], 1144201745"
      }

      "correctly encode test DWORD PTR cs:[rbx], 0x44332211" in {
        Test(0x44332211, RegisterMemoryLocation[DoubleWordSize](CS/RBX)).encodeByte should be(Hex.lsb("2e F7 03 11 22 33 44"))
      }

      "correctly represent test DWORD PTR cs:[rbx], 0x44332211 as a string" in {
        Test(0x44332211, RegisterMemoryLocation[DoubleWordSize](CS/RBX)).toString shouldBe "test DWORD PTR cs:[rbx], 1144201745"
      }

      "correctly encode test WORD PTR cs:[rbx], 0x2211" in {
        Test(0x2211.toShort, RegisterMemoryLocation[WordSize](CS/RBX)).encodeByte should be(Hex.lsb("2e 66 F7 03 11 22"))
      }

      "correctly represent test WORD PTR cs:[rbx], 0x2211 as a string" in {
        Test(0x2211.toShort, RegisterMemoryLocation[WordSize](CS/RBX)).toString shouldBe "test WORD PTR cs:[rbx], 8721"
      }

      "correctly encode test BYTE PTR cs:[rbx], 0x11" in {
        Test(0x11.toByte, RegisterMemoryLocation[ByteSize](CS/RBX)).encodeByte should be(Hex.lsb("2e F6 03 11"))
      }

      "correctly represent test BYTE PTR cs:[rbx], 0x11 as a string" in {
        Test(0x11.toByte, RegisterMemoryLocation[ByteSize](CS/RBX)).toString shouldBe "test BYTE PTR cs:[rbx], 17"
      }


      "correctly encode test rbx, 0x44332211" in {
        Test(0x44332211, RBX).encodeByte should be(Hex.lsb("48 F7 C3 11 22 33 44"))
      }

      "correctly represent test rbx, 0x44332211 as a string" in {
        Test(0x44332211, RBX).toString shouldBe "test rbx, 1144201745"
      }

      "correctly encode test DWORD PTR [rax+rbx*2], 0x44" in {
        an[AssertionError] should be thrownBy {
          Test(0x44.toByte, SIBMemoryLocation[DoubleWordSize](RBX, RAX, scale = 2))
        }
      }

      "correctly encode test QWORD PTR [rax+rbx*2], 0x44" in {
        an[AssertionError] should be thrownBy {
          Test(0x44.toByte, SIBMemoryLocation[QuadWordSize](RBX, RAX, scale = 2))
        }
      }

      "correctly encode test BYTE PTR [rbx], al" in {
        Test(AL, RegisterMemoryLocation[ByteSize](RBX)).encodeByte should be(Hex.lsb("84 03"))
      }

      "correctly represent test BYTE PTR [rbx], al as a string" in {
        Test(AL, RegisterMemoryLocation[ByteSize](RBX)).toString shouldBe "test BYTE PTR [rbx], al"
      }

      "correctly encode test BYTE PTR [rbx], ah" in {
        Test(AH, RegisterMemoryLocation[ByteSize](RBX)).encodeByte should be(Hex.lsb("84 23"))
      }

      "correctly represent test BYTE PTR [rbx], ah as a string" in {
        Test(AH, RegisterMemoryLocation[ByteSize](RBX)).toString shouldBe "test BYTE PTR [rbx], ah"
      }

      "correctly encode test BYTE PTR [rbx], r15l" in {
        Test(R15L, RegisterMemoryLocation[ByteSize](RBX)).encodeByte should be(Hex.lsb("44 84 3B"))
      }

      "correctly represent test BYTE PTR [rbx], r15l as a string" in {
        Test(R15L, RegisterMemoryLocation[ByteSize](RBX)).toString shouldBe "test BYTE PTR [rbx], r15l"
      }

      "correctly encode test QWORD PTR [rax], rax" in {
        Test(RAX, RegisterMemoryLocation[QuadWordSize](RAX)).encodeByte should be(Hex.lsb("48 85 00"))
      }

      "correctly represent test QWORD PTR [rax], rax as a string" in {
        Test(RAX, RegisterMemoryLocation[QuadWordSize](RAX)).toString shouldBe "test QWORD PTR [rax], rax"
      }

      "correctly encode test RAX, RDI" in {
        Test(RDI, RAX).encodeByte should be(Hex.lsb("48 85 f8"))
      }

      "correctly represent test RAX, RDI as a string" in {
        Test(RDI, RAX).toString shouldBe "test rax, rdi"
      }
    }
  }
}
