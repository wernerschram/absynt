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
import org.werner.absynt.x86.operands.{ByteSize, DoubleWordSize, QuadWordSize, WordSize}
import org.werner.absynt.x86.operands.memoryaccess._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ArithmeticSuite extends AnyWordSpec with Matchers {

  // ADC, ADD, AND, CMP, OR, SBB, SUB and XOR all inherits from BasicInteraction.
  // BasicInteraction is covered by the XOR tests, for the others there are some testcases to test the opcode.

  "an AddCarry instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encodeByte adc al, 0x40" in {
        AddCarry(0x40.toByte, AL).encodeByte should be(Hex.lsb("14 40"))
      }

      "correctly encodeByte adc bl, 0x40" in {
        AddCarry(0x40.toByte, BL).encodeByte should be(Hex.lsb("80 D3 40"))
      }
    }
  }

  "an Add instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode add al, 0x40" in {
        Add(0x40.toByte, AL).encodeByte should be(Hex.lsb("04 40"))
      }

      "correctly encode add bl, 0x40" in {
        Add(0x40.toByte, BL).encodeByte should be(Hex.lsb("80 C3 40"))
      }
    }
  }

  "an And instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode and al, 0x40" in {
        And(0x40.toByte, AL).encodeByte should be(Hex.lsb("24 40"))
      }

      "correctly encode and bl, 0x40" in {
        And(0x40.toByte, BL).encodeByte should be(Hex.lsb("80 E3 40"))
      }
    }
  }

  "an Compare instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode cmp al, 0x40" in {
        Compare(0x40.toByte, AL).encodeByte should be(Hex.lsb("3C 40"))
      }

      "correctly encode cmp bl, 0x40" in {
        Compare(0x40.toByte, BL).encodeByte should be(Hex.lsb("80 FB 40"))
      }
    }
  }

  "an Not instruction" when {

    "in legacy mode" should {

      import ProcessorMode.Legacy._

      "correctly encode not BYTE PTR [0x01]" in {
        Not(MemoryAddress[ByteSize](0x0001.toShort)).encodeByte should be(Hex.lsb("F6 16 01 00"))
      }

      "correctly encode not WORD PTR [0x0001]" in {
        Not(MemoryAddress[WordSize](0x0001.toShort)).encodeByte should be(Hex.lsb("F7 16 01 00"))
      }
    }

    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode not BYTE PTR [0x01]" in {
        Not(MemoryAddress[ByteSize](0x0001.toShort)).encodeByte should be(Hex.lsb("F6 16 01 00"))
      }

      "correctly encode not WORD PTR [0x0001]" in {
        Not(MemoryAddress[WordSize](0x0001.toShort)).encodeByte should be(Hex.lsb("F7 16 01 00"))
      }
    }
    "in protected mode" should {

      import ProcessorMode.Protected._

      "correctly encode not eax" in {
        Not(EAX).encodeByte should be(Hex.lsb("F7 D0"))
      }
    }

    "in long mode" should {

      import ProcessorMode.Long._

      "correctly encode not QWORD PTR [rax]" in {
        Not(RegisterMemoryLocation[QuadWordSize](RAX)).encodeByte should be(Hex.lsb("48 F7 10"))
      }
    }
  }

  "an Or instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode or al, 0x40" in {
        Or(0x40.toByte, AL).encodeByte should be(Hex.lsb("0C 40"))
      }

      "correctly encode or bl, 0x40" in {
        Or(0x40.toByte, BL).encodeByte should be(Hex.lsb("80 CB 40"))
      }
    }
  }

  "an SubtractBorrow instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode sbb al, 0x40" in {
        SubtractBorrow(0x40.toByte, AL).encodeByte should be(Hex.lsb("1C 40"))
      }

      "correctly encode sbb bl, 0x40" in {
        SubtractBorrow(0x40.toByte, BL).encodeByte should be(Hex.lsb("80 DB 40"))
      }
    }
  }

  "an Subtract instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode sub al, 0x40" in {
        Subtract(0x40.toByte, AL).encodeByte should be(Hex.lsb("2C 40"))
      }

      "correctly encode sub bl, 0x40" in {
        Subtract(0x40.toByte, BL).encodeByte should be(Hex.lsb("80 EB 40"))
      }
    }
  }

  "an Xor instruction" when {

    "in legacy mode" should {
      import ProcessorMode.Legacy._

      "correctly encode xor bl, 0x40" in {
        Xor(0x40.toByte, BL).encodeByte should be(Hex.lsb("80 F3 40"))
      }


    }

    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode xor al, 0x40" in {
        Xor(0x40.toByte, AL).encodeByte should be(Hex.lsb("34 40"))
      }

      "correctly represent xor al, 0x40 as a string" in {
        Xor(0x40.toByte, AL).toString shouldBe "xor al, 64"
      }

      "correctly encode xor ax, 0x3412" in {
        Xor(0x3412.toShort, AX).encodeByte should be(Hex.lsb("35 12 34"))
      }

      "correctly represent xor ax, 0x3412 as a string" in {
        Xor(0x3412.toShort, AX).toString shouldBe "xor ax, 13330"
      }

      info("xor AL, AL can be encoded with -xor r/m8, r8- (30 C0) and with -xor r8, r/m8- (32 C0)")
      info("These are equal in every aspect. -xor r/m8, r8- is chosen to be the correct encoding")

      "correctly encode xor al, al" in {
        Xor(AL, AL).encodeByte should be(Hex.lsb("30 C0"))
        Add(EAX, EAX)
      }

      "correctly represent xor al, al as a string" in {
        Xor(AL, AL).toString shouldBe "xor al, al"
      }

      info("xor AX, AX can be encoded with -xor r/m16, r16- (31 C0) and with -xor r16, r/m16- (33 C0)")
      info("These are equal in every aspect. -xor r/m16, r16- is chosen to be the correct encoding")

      "correctly encode xor ax, ax" in {
        Xor(AX, AX).encodeByte should be(Hex.lsb("31 C0"))
      }

      "correctly represent xor ax, ax as a string" in {
        Xor(AX, AX).toString shouldBe "xor ax, ax"
      }

      "correctly encode xor eax, 0x44332211" in {
        Xor(0x44332211, EAX).encodeByte should be(Hex.lsb("66 35 11 22 33 44"))
      }

      "correctly represent xor ax, 0x44332211 as a string" in {
        Xor(0x44332211, EAX).toString shouldBe "xor eax, 1144201745"
      }

      "correctly encode xor bl, 0x40" in {
        Xor(0x40.toByte, BL).encodeByte should be(Hex.lsb("80 F3 40"))
      }

      "correctly represent xor bl, 0x40 as a string" in {
        Xor(0x40.toByte, BL).toString shouldBe "xor bl, 64"
      }

      "correctly encode xor WORD PTR [bx], 0x3412" in {
        Xor(0x3412.toShort, RegisterMemoryLocation[WordSize](BX)).encodeByte should be(Hex.lsb("81 37 12 34"))
      }

      "correctly represent xor WORD PTR [bx], 0x3412 as a string" in {
        Xor(0x3412.toShort, RegisterMemoryLocation[WordSize](BX)).toString shouldBe "xor WORD PTR [bx], 13330"
      }

      "correctly encode xor BYTE PTR [bx], al" in {
        Xor(AL, RegisterMemoryLocation[ByteSize](BX)).encodeByte should be(Hex.lsb("30 07"))
      }

      "correctly represent xor BYTE PTR [bx], al as a string" in {
        Xor(AL, RegisterMemoryLocation[ByteSize](BX)).toString shouldBe "xor BYTE PTR [bx], al"
      }

      "correctly encode xor ah, BYTE PTR [si]" in {
        Xor(RegisterMemoryLocation[ByteSize](SI), AH).encodeByte should be(Hex.lsb("32 24"))
      }

      "correctly represent xor ah, BYTE PTR [si] as a string" in {
        Xor(RegisterMemoryLocation[ByteSize](SI), AH).toString shouldBe "xor ah, BYTE PTR [si]"
      }

    }

    "in protected mode" should {

      import ProcessorMode.Protected._

      "correctly encode xor DWORD PTR [0x11111111], 0x44332211" in {
        Xor(0x44332211, MemoryAddress[DoubleWordSize](0x11111111)).encodeByte should be(Hex.lsb("81 35 11 11 11 11 11 22 33 44"))
      }

      "correctly represent xor DWORD PTR [0x11111111], 0x44332211 as a string" in {
        Xor(0x44332211, MemoryAddress[DoubleWordSize](0x11111111)).toString shouldBe "xor DWORD PTR [286331153], 1144201745"
      }

      "correctly encode xor WORD PTR [0x11111111], 0x44" in {
        Xor(0x44.toByte, MemoryAddress[WordSize](0x11111111)).encodeByte should be(Hex.lsb("66 83 35 11 11 11 11 44"))
      }

      "correctly represent xor WORD PTR [0x11111111], 0x44 as a string" in {
        Xor(0x44.toByte, MemoryAddress[WordSize](0x11111111)).toString shouldBe "xor WORD PTR [286331153], 68"
      }

      "correctly encode xor DWORD PTR [0x1234], edx" in {
        Xor(EDX, MemoryAddress[DoubleWordSize](0X1234.toShort)).encodeByte should be(Hex.lsb("67 31 16 34 12"))
      }

      "correctly represent xor DWORD PTR [0x1234], edx as a string" in {
        Xor(EDX, MemoryAddress[DoubleWordSize](0x1234.toShort)).toString shouldBe "xor DWORD PTR [4660], edx"
      }

      "correctly encode xor esi, DWORD PTR gs:[eax+0x12]" in {
        Xor(RegisterMemoryLocation.withSegmentOverride[DoubleWordSize](EAX, 0x12.toByte, GS), ESI).encodeByte should be(Hex.lsb("65 33 70 12"))
      }

      "correctly represent xor esi, DWORD PTR gs:[eax+0x12] as a string" in {
        Xor(RegisterMemoryLocation.withSegmentOverride[DoubleWordSize](EAX, 0x12.toByte, GS), ESI).toString shouldBe "xor esi, DWORD PTR gs:[eax+18]"
      }

    }

    "in long mode" should {

      import ProcessorMode.Long._

      "correctly encode xor QWORD PTR [0x11111111], 0x44332211" in {
        Xor(0x44332211, MemoryAddress[QuadWordSize](0x11111111)).encodeByte should be(Hex.lsb("67 48 81 35 11 11 11 11 11 22 33 44"))
      }

      "correctly represent xor QWORD PTR [0x11111111], 0x44332211 as a string" in {
        Xor(0x44332211, MemoryAddress[QuadWordSize](0x11111111)).toString shouldBe "xor QWORD PTR [286331153], 1144201745"
      }

      "throw an AssertionError for xor WORD PTR [0x11111111], 0x44332211" in {
        an[AssertionError] should be thrownBy {
          Xor(0x44332211, MemoryAddress[WordSize](0x11111111))
        }
      }

      "correctly encode xor rax, 0x78776655" in {
        Xor(0x78776655, RAX).encodeByte should be(Hex.lsb("48 35 55 66 77 78"))
      }

      "correctly represent xor rax, 0x78776655 as a string" in {
        Xor(0x78776655, RAX).toString shouldBe "xor rax, 2021090901"
      }

      // Note that the GNU assembler (and likely others) denotes this as xor RAX, 0xFFFFFFFF88776655 and doens't accept this notation.
      "correctly encode xor rax, 0x88776655" in {
        Xor(0x88776655, RAX).encodeByte should be(Hex.lsb("48 35 55 66 77 88"))
      }

      "correctly represent xor rax, 0x88776655 as a string" in {
        Xor(0x88776655, RAX).toString shouldBe "xor rax, -2005440939"
      }

      "correctly encode xor BYTE PTR [rax+rbx*2], 0x11" in {
        Xor(0x11.toByte, SIBMemoryLocation[ByteSize](RBX, RAX, scale = 2)).encodeByte should be(Hex.lsb("80 34 58 11"))
      }

      "correctly represent xor BYTE PTR [rax+rbx*2], 0x11 as a string" in {
        Xor(0x11.toByte, SIBMemoryLocation[ByteSize](RBX, RAX, scale = 2)).toString shouldBe "xor BYTE PTR [rax+rbx*2], 17"
      }

      "correctly encode xor WORD PTR [rax+rbx*2], 0x2211" in {
        Xor(0x2211.toShort, SIBMemoryLocation[WordSize](RBX, RAX, scale = 2)).encodeByte should be(Hex.lsb("66 81 34 58 11 22"))
      }

      "correctly represent xor WORD PTR [rax+rbx*2], 0x2211 as a string" in {
        Xor(0x2211.toShort, SIBMemoryLocation[WordSize](RBX, RAX, scale = 2)).toString shouldBe "xor WORD PTR [rax+rbx*2], 8721"
      }

      "correctly encode xor DWORD PTR [rax+rbx*2], 0x44332211" in {
        Xor(0x44332211, SIBMemoryLocation[DoubleWordSize](RBX, RAX, scale = 2)).encodeByte should be(Hex.lsb("81 34 58 11 22 33 44"))
      }

      "correctly represent xor DWORD PTR [rax+rbx*2], 0x44332211 as a string" in {
        Xor(0x44332211, SIBMemoryLocation[DoubleWordSize](RBX, RAX, scale = 2)).toString shouldBe "xor DWORD PTR [rax+rbx*2], 1144201745"
      }

      "correctly encode xor QWORD PTR [rax+rbx*2], 0x44332211" in {
        Xor(0x44332211, SIBMemoryLocation[QuadWordSize](RBX, RAX, scale = 2)).encodeByte should be(Hex.lsb("48 81 34 58 11 22 33 44"))
      }

      "correctly represent xor QWORD PTR [rax+rbx*2], 0x44332211 as a string" in {
        Xor(0x44332211, SIBMemoryLocation[QuadWordSize](RBX, RAX, scale = 2)).toString shouldBe "xor QWORD PTR [rax+rbx*2], 1144201745"
      }

      "correctly encode xor BYTE PTR gs:[rax+rbx*2], 0x11" in {
        Xor(0x11.toByte, SIBMemoryLocation.withSegmentOverride[ByteSize](RBX, RAX, scale = 2, segment = GS)).encodeByte should be(Hex.lsb("65 80 34 58 11"))
      }

      "correctly represent xor BYTE PTR gs:[rax+rbx*2], 0x11 as a string" in {
        Xor(0x11.toByte, SIBMemoryLocation.withSegmentOverride[ByteSize](RBX, RAX, scale = 2, segment = GS)).toString shouldBe "xor BYTE PTR gs:[rax+rbx*2], 17"
      }

      "correctly encode xor WORD PTR es:[rax+rbx*2], 0x2211" in {
        Xor(0x2211.toShort, SIBMemoryLocation.withSegmentOverride[WordSize](RBX, RAX, scale = 2, segment = ES)).encodeByte should be(Hex.lsb("26 66 81 34 58 11 22"))
      }

      "correctly represent xor WORD PTR es:[rax+rbx*2], 0x2211 as a string" in {
        Xor(0x2211.toShort, SIBMemoryLocation.withSegmentOverride[WordSize](RBX, RAX, scale = 2, segment = ES)).toString shouldBe "xor WORD PTR es:[rax+rbx*2], 8721"
      }

      "correctly encode xor DWORD PTR fs:[rax+rbx*2], 0x44332211" in {
        Xor(0x44332211, SIBMemoryLocation.withSegmentOverride[DoubleWordSize](RBX, RAX, scale = 2, segment = FS)).encodeByte should be(Hex.lsb("64 81 34 58 11 22 33 44"))
      }

      "correctly represent xor DWORD PTR fs:[rax+rbx*2], 0x44332211 as a string" in {
        Xor(0x44332211, SIBMemoryLocation.withSegmentOverride[DoubleWordSize](RBX, RAX, scale = 2, segment = FS)).toString shouldBe "xor DWORD PTR fs:[rax+rbx*2], 1144201745"
      }

      "correctly encode xor QWORD PTR ss:[rax+rbx*2], 0x44332211" in {
        Xor(0x44332211, SIBMemoryLocation.withSegmentOverride[QuadWordSize](RBX, RAX, scale = 2, segment = SS)).encodeByte should be(Hex.lsb("36 48 81 34 58 11 22 33 44"))
      }

      "correctly represent xor QWORD PTR ss:[rax+rbx*2], 0x44332211 as a string" in {
        Xor(0x44332211, SIBMemoryLocation.withSegmentOverride[QuadWordSize](RBX, RAX, scale = 2, segment = SS)).toString shouldBe "xor QWORD PTR ss:[rax+rbx*2], 1144201745"
      }

      "correctly encode xor QWORD PTR cs:[eax], 0x44332211" in {
        Xor(0x44332211, RegisterMemoryLocation.withSegmentOverride[QuadWordSize](EAX, segment = CS)).encodeByte should be(Hex.lsb("2e 67 48 81 30 11 22 33 44"))
      }

      "correctly represent xor QWORD PTR cs:[eax], 0x44332211 as a string" in {
        Xor(0x44332211, RegisterMemoryLocation.withSegmentOverride[QuadWordSize](EAX, segment = CS)).toString shouldBe "xor QWORD PTR cs:[eax], 1144201745"
      }

      "correctly encode xor DWORD PTR cs:[rbx], 0x44332211" in {
        Xor(0x44332211, RegisterMemoryLocation.withSegmentOverride[DoubleWordSize](RBX, segment = CS)).encodeByte should be(Hex.lsb("2e 81 33 11 22 33 44"))
      }

      "correctly represent xor DWORD PTR cs:[rbx], 0x44332211 as a string" in {
        Xor(0x44332211, RegisterMemoryLocation.withSegmentOverride[DoubleWordSize](RBX, segment = CS)).toString shouldBe "xor DWORD PTR cs:[rbx], 1144201745"
      }

      "correctly encode xor WORD PTR cs:[rbx], 0x2211" in {
        Xor(0x2211.toShort, RegisterMemoryLocation.withSegmentOverride[WordSize](RBX, segment = CS)).encodeByte should be(Hex.lsb("2e 66 81 33 11 22"))
      }

      "correctly represent xor WORD PTR cs:[rbx], 0x2211 as a string" in {
        Xor(0x2211.toShort, RegisterMemoryLocation.withSegmentOverride[WordSize](RBX, segment = CS)).toString shouldBe "xor WORD PTR cs:[rbx], 8721"
      }

      "correctly encode xor BYTE PTR cs:[rbx], 0x11" in {
        Xor(0x11.toByte, RegisterMemoryLocation.withSegmentOverride[ByteSize](RBX, segment = CS)).encodeByte should be(Hex.lsb("2e 80 33 11"))
      }

      "correctly represent xor BYTE PTR cs:[rbx], 0x11 as a string" in {
        Xor(0x11.toByte, RegisterMemoryLocation.withSegmentOverride[ByteSize](RBX, segment = CS)).toString shouldBe "xor BYTE PTR cs:[rbx], 17"
      }


      "correctly encode xor rbx, 0x44332211" in {
        Xor(0x44332211, RBX).encodeByte should be(Hex.lsb("48 81 F3 11 22 33 44"))
      }

      "correctly represent xor rbx, 0x44332211 as a string" in {
        Xor(0x44332211, RBX).toString shouldBe "xor rbx, 1144201745"
      }

      "correctly encode xor DWORD PTR [rax+rbx*2], 0x44" in {
        Xor(0x44.toByte, SIBMemoryLocation[DoubleWordSize](RBX, RAX, scale = 2)).encodeByte should be(Hex.lsb("83 34 58 44"))
      }

      "correctly represent xor DWORD PTR [rax+rbx*2], 0x44 as a string" in {
        Xor(0x44.toByte, SIBMemoryLocation[DoubleWordSize](RBX, RAX, scale = 2)).toString shouldBe "xor DWORD PTR [rax+rbx*2], 68"
      }

      "correctly encode xor QWORD PTR [rax+rbx*2], 0x44" in {
        Xor(0x44.toByte, SIBMemoryLocation[QuadWordSize](RBX, RAX, scale = 2)).encodeByte should be(Hex.lsb("48 83 34 58 44"))
      }

      "correctly represent xor QWORD PTR [rax+rbx*2], 0x44 as a string" in {
        Xor(0x44.toByte, SIBMemoryLocation[QuadWordSize](RBX, RAX, scale = 2)).toString shouldBe "xor QWORD PTR [rax+rbx*2], 68"
      }

      "correctly encode xor BYTE PTR [rbx], al" in {
        Xor(AL, RegisterMemoryLocation[ByteSize](RBX)).encodeByte should be(Hex.lsb("30 03"))
      }

      "correctly represent xor BYTE PTR [rbx], al as a string" in {
        Xor(AL, RegisterMemoryLocation[ByteSize](RBX)).toString shouldBe "xor BYTE PTR [rbx], al"
      }

      "correctly encode xor BYTE PTR [rbx], ah" in {
        Xor(AH, RegisterMemoryLocation[ByteSize](RBX)).encodeByte should be(Hex.lsb("30 23"))
      }

      "correctly represent xor BYTE PTR [rbx], ah as a string" in {
        Xor(AH, RegisterMemoryLocation[ByteSize](RBX)).toString shouldBe "xor BYTE PTR [rbx], ah"
      }

      "correctly encode xor BYTE PTR [rbx], r15l" in {
        Xor(R15L, RegisterMemoryLocation[ByteSize](RBX)).encodeByte should be(Hex.lsb("44 30 3B"))
      }

      "correctly represent xor BYTE PTR [rbx], r15l as a string" in {
        Xor(R15L, RegisterMemoryLocation[ByteSize](RBX)).toString shouldBe "xor BYTE PTR [rbx], r15l"
      }

      "correctly encode xor QWORD PTR [rax], rax" in {
        Xor(RAX, RegisterMemoryLocation[QuadWordSize](RAX)).encodeByte should be(Hex.lsb("48 31 00"))
      }

      "correctly represent xor QWORD PTR [rax], rax as a string" in {
        Xor(RAX, RegisterMemoryLocation[QuadWordSize](RAX)).toString shouldBe "xor QWORD PTR [rax], rax"
      }

      "correctly encode xor RAX, RDI" in {
        Xor(RDI, RAX).encodeByte should be(Hex.lsb("48 31 f8"))
      }

      "correctly represent xor RAX, RDI as a string" in {
        Xor(RDI, RAX).toString shouldBe "xor rax, rdi"
      }

      "correctly encode xor al, BYTE PTR [rax]" in {
        Xor(RegisterMemoryLocation[ByteSize](RAX), AL).encodeByte should be(Hex.lsb("32 00"))
      }

      "correctly represent xor al, BYTE PTR [rax] as a string" in {
        Xor(RegisterMemoryLocation[ByteSize](RAX), AL).toString shouldBe "xor al, BYTE PTR [rax]"
      }

      "correctly encode xor r15, DWORD PTR [r11]" in {
        Xor(RegisterMemoryLocation[DoubleWordSize](R11), R15).encodeByte should be(Hex.lsb("4D 33 3B"))
      }

      "correctly represent xor r15, DWORD PTR [r11] as a string" in {
        Xor(RegisterMemoryLocation[DoubleWordSize](R11), R15).toString shouldBe "xor r15, DWORD PTR [r11]"
      }
    }
  }
}
