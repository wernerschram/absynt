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

import org.werner.absynt._
import org.werner.absynt.output.raw.Raw
import org.werner.absynt.resource.Resource
import org.werner.absynt.sections.Section
import org.werner.absynt.x86.ProcessorMode
import org.werner.absynt.x86.operands.{ByteSize, DoubleWordSize, QuadWordSize, WordSize}
import org.werner.absynt.x86.operands.memoryaccess._
import org.scalatest.{Matchers, WordSpec}

class MoveSuite extends WordSpec with Matchers {

  "a Move instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode mov bh, al" in {
        Move(AL, BH).encodeByte should be(Hex.lsb("88 C7"))
      }

      "correctly represent mov bh, al as a string" in {
        Move(AL, BH).toString should be("mov bh, al")
      }

      "correctly encode mov BYTE PTR [si], ch" in {
        Move(CH, RegisterMemoryLocation[ByteSize](SI)).encodeByte should be(Hex.lsb("88 2C"))
      }

      "correctly represent mov BYTE PTR [si], ch as a string" in {
        Move(CH, RegisterMemoryLocation[ByteSize](SI)).toString should be("mov BYTE PTR [si], ch")
      }

      "correctly encode mov BYTE PTR fs:[si], ch" in {
        Move(CH, RegisterMemoryLocation.withSegmentOverride[ByteSize](SI, segment = FS)).encodeByte should be(Hex.lsb("64 88 2C"))
      }

      "correctly represent mov BYTE PTR fs:[si], ch as a string" in {
        Move(CH, RegisterMemoryLocation.withSegmentOverride[ByteSize](SI, segment = FS)).toString should be("mov BYTE PTR fs:[si], ch")
      }

      "correctly encode BYTE PTR mov cs:[di], ch" in {
        Move(CH, RegisterMemoryLocation.withSegmentOverride[ByteSize](DI, segment = CS)).encodeByte should be(Hex.lsb("2E 88 2D"))
      }

      "correctly encode mov BYTE PTR [bp+si+0x7D], bh" in {
        Move(BH, RegisterMemoryLocation[ByteSize](BP+SI, 0x7D.toByte)).encodeByte should be(Hex.lsb("88 7A 7D"))
      }

      "correctly represent mov BYTE PTR [bp+si+0x7D], bh as a string" in {
        Move(BH, RegisterMemoryLocation[ByteSize](SI+BP, 0x7D.toByte)).toString should be("mov BYTE PTR [bp+si+125], bh")
      }

     "correctly encode mov WORD PTR [bp], bp" in {
        Move(BP, RegisterMemoryLocation[WordSize](BP)).encodeByte should be(Hex.lsb("89 6E 00"))
      }

      "correctly represent mov WORD PTR [bp], bp as a string" in {
        Move(BP, RegisterMemoryLocation[WordSize](BP)).toString should be("mov WORD PTR [bp], bp")
      }

      "correctly encode mov BYTE PTR [bp]+0x1234, cl" in {
        Move(CL, RegisterMemoryLocation[ByteSize](BP, 0x1234.toShort)).encodeByte should be(Hex.lsb("88 8E 34 12"))
      }

      "correctly represent mov BYTE PTR [bp]+0x1234, cl as a string" in {
        Move(CL, RegisterMemoryLocation[ByteSize](BP, 0x1234.toShort)).toString should be("mov BYTE PTR [bp+4660], cl")
      }

      "correctly encode mov bx, ax" in {
        Move(AX, BX).encodeByte should be(Hex.lsb("89 C3"))
      }

      "correctly represent mov bx, ax as a string" in {
        Move(AX, BX).toString should be("mov bx, ax")
      }

      "correctly encode mov ecx, edx" in {
        Move(EDX, ECX).encodeByte should be(Hex.lsb("66 89 D1"))
      }

      "correctly represent mov ecx, edx as a string" in {
        Move(EDX, ECX).toString should be("mov ecx, edx")
      }

      "correctly encode mov WORD PTR [si], si" in {
        Move(SI, RegisterMemoryLocation[WordSize](SI)).encodeByte should be(Hex.lsb("89 34"))
      }

      "correctly represent mov WORD PTR [si], si as a string" in {
        Move(SI, RegisterMemoryLocation[WordSize](SI)).toString should be("mov WORD PTR [si], si")
      }

      "correctly encode mov DWORD PTR [ecx+ebx*1], edx" in {
        Move(EDX, SIBMemoryLocation[DoubleWordSize](EBX, ECX, 1)).encodeByte should be(Hex.lsb("67 66 89 14 19"))
      }

      "correctly represent mov DWORD PTR [ecx+ebx*1], edx as a string" in {
        Move(EDX, SIBMemoryLocation[DoubleWordSize](EBX, ECX, 1)).toString should be("mov DWORD PTR [ecx+ebx*1], edx")
      }

      "correctly encode mov DWORD PTR [ecx+ebx*4], edx" in {
        Move(EDX, SIBMemoryLocation[DoubleWordSize](EBX, ECX, scale = 4)).encodeByte should be(Hex.lsb("67 66 89 14 99"))
      }

      "correctly represent mov DWORD PTR [ecx+ebx*4], edx as a string" in {
        Move(EDX, SIBMemoryLocation[DoubleWordSize](EBX, ECX, scale = 4)).toString should be("mov DWORD PTR [ecx+ebx*4], edx")
      }

      "correctly encode mov DWORD PTR gs:[ecx+ebx*4], edx" in {
        Move(EDX, SIBMemoryLocation.withSegmentOverride[DoubleWordSize](EBX, ECX, scale = 4, segment = GS)).encodeByte should be(Hex.lsb("65 67 66 89 14 99"))
      }

      "correctly represent mov DWORD PTR gs:[ecx+ebx*4], edx as a string" in {
        Move(EDX, SIBMemoryLocation.withSegmentOverride[DoubleWordSize](EBX, ECX, scale = 4, segment = GS)).toString should be("mov DWORD PTR gs:[ecx+ebx*4], edx")
      }

      "correctly encode mov DWORD PTR [ecx+ebx*4], eax" in {
        Move(EAX, SIBMemoryLocation[DoubleWordSize](EBX, ECX, scale = 4)).encodeByte should be(Hex.lsb("67 66 89 04 99"))
      }

      "correctly represent mov DWORD PTR [ecx+ebx*4], eax as a string" in {
        Move(EAX, SIBMemoryLocation[DoubleWordSize](EBX, ECX, scale = 4)).toString should be("mov DWORD PTR [ecx+ebx*4], eax")
      }

      "correctly encode mov DWORD PTR [edi+edx*8+0x0110], ecx" in {
        Move(ECX, SIBMemoryLocation[DoubleWordSize](EDX, EDI, 0x0110, 8)).encodeByte should be(Hex.lsb("67 66 89 8C D7 10 01 00 00"))
      }

      "correctly represent mov DWORD PTR [edi+edx*8+272], ecx as a string" in {
        Move(ECX, SIBMemoryLocation[DoubleWordSize](EDX, EDI, 0x0110, 8)).toString should be("mov DWORD PTR [edi+edx*8+272], ecx")
      }

      "correctly encode mov WORD PTR [bx+3D], bp" in {
        Move(BP, RegisterMemoryLocation[WordSize](BX, 0x3D.toByte)).encodeByte should be(Hex.lsb("89 6F 3D"))
      }

      "correctly represent mov WORD PTR [bx+61], bp as a string" in {
        Move(BP, RegisterMemoryLocation[WordSize](BX, 0x3D.toByte)).toString should be("mov WORD PTR [bx+61], bp")
      }

      "correctly encode mov WORD PTR [bp+di+0xDEAD], cx" in {
        Move(CX, RegisterMemoryLocation[WordSize](BP+DI, 0xDEAD.toShort)).encodeByte should be(Hex.lsb("89 8B AD DE"))
      }

      "correctly represent mov WORD PTR [bp+di+57005], cx as a string" in {
        Move(CX, RegisterMemoryLocation[WordSize](BP+DI, 0xDEAD.toShort)).toString should be("mov WORD PTR [bp+di+57005], cx")
      }

      "correctly encode mov cl, BYTE PTR [bp+si]" in {
        Move(RegisterMemoryLocation[ByteSize](BP+SI), CL).encodeByte should be(Hex.lsb("8A 0A"))
      }

      "correctly represent mov cl, BYTE PTR [bp+si] as a string" in {
        Move(RegisterMemoryLocation[ByteSize](BP+SI), CL).toString should be("mov cl, BYTE PTR [bp+si]")
      }

      "correctly encode mov dl, BYTE PTR [bx+di+0xA0]" in {
        Move(RegisterMemoryLocation[ByteSize](BX+DI, 0xA0.toByte), DL).encodeByte should be(Hex.lsb("8A 51 A0"))
      }

      "correctly represent mov dl, BYTE PTR [bx+di+160] as a string" in {
        Move(RegisterMemoryLocation[ByteSize](BX+DI, 0xA0.toByte), DL).toString should be("mov dl, BYTE PTR [bx+di+160]")
      }

      "correctly encode mov dh, BYTE PTR [bp]+0xABBA" in {
        Move(RegisterMemoryLocation[ByteSize](BP, 0xABBA.toShort), DH).encodeByte should be(Hex.lsb("8A B6 BA AB"))
      }

      "correctly represent dh, BYTE PTR [bp+43962] as a string" in {
        Move(RegisterMemoryLocation[ByteSize](BP, 0xABBA.toShort), DH).toString should be("mov dh, BYTE PTR [bp+43962]")
      }

      "correctly encode mov dx, WORD PTR [si]" in {
        Move(RegisterMemoryLocation[WordSize](SI), DX).encodeByte should be(Hex.lsb("8B 14"))
      }

      "correctly represent mov dx, WORD PTR [si] as a string" in {
        Move(RegisterMemoryLocation[WordSize](SI), DX).toString should be("mov dx, WORD PTR [si]")
      }

      "correctly encode mov sp, WORD PTR [bp+di+0x12]" in {
        Move(RegisterMemoryLocation[WordSize](BP+DI, 0x12.toByte), SP).encodeByte should be(Hex.lsb("8B 63 12"))
      }

      "correctly represent mov sp, WORD PTR [bp+di+18] as a string" in {
        Move(RegisterMemoryLocation[WordSize](BP+DI, 0x12.toByte), SP).toString should be("mov sp, WORD PTR [bp+di+18]")
      }

      "correctly encode mov di, WORD PTR [di+0xBEEF]" in {
        Move(RegisterMemoryLocation[WordSize](DI, 0xBEEF.toShort), DI).encodeByte should be(Hex.lsb("8B BD EF BE"))
      }

      "correctly represent mov di, WORD PTR [di+48879] as a string" in {
        Move(RegisterMemoryLocation[WordSize](DI, 0xBEEF.toShort), DI).toString should be("mov di, WORD PTR [di+48879]")
      }

      "correctly encode mov dx, cs" in {
        Move(CS, DX).encodeByte should be(Hex.lsb("8C CA"))
      }

      "correctly represent mov dx, cs as a string" in {
        Move(CS, DX).toString should be("mov dx, cs")
      }

      "correctly encode mov WORD PTR [bx], es" in {
        Move(ES, RegisterMemoryLocation[WordSize](BX)).encodeByte should be(Hex.lsb("8C 07"))
      }

      "correctly represent mov WORD PTR [bx], es as a string" in {
        Move(ES, RegisterMemoryLocation[WordSize](BX)).toString should be("mov WORD PTR [bx], es")
      }

      "correctly encode mov WORD PTR [si+0x1234], fs" in {
        Move(FS, RegisterMemoryLocation[WordSize](SI, 0x1234.toShort)).encodeByte should be(Hex.lsb("8C A4 34 12"))
      }

      "correctly represent mov WORD PTR [si+4660], fs as a string" in {
        Move(FS, RegisterMemoryLocation[WordSize](SI, 0x1234.toShort)).toString should be("mov WORD PTR [si+4660], fs")
      }

      "correctly encode mov gs, si" in {
        Move(SI, GS).encodeByte should be(Hex.lsb("8E EE"))
      }

      "correctly represent mov gs, si as a string" in {
        Move(SI, GS).toString should be("mov gs, si")
      }

      "correctly encode mov ss, WORD PTR [bp+si]" in {
        Move(RegisterMemoryLocation[WordSize](BP+SI), SS).encodeByte should be(Hex.lsb("8E 12"))
      }

      "correctly represent mov ss, WORD PTR [bp+si] as a string" in {
        Move(RegisterMemoryLocation[WordSize](BP+SI), SS).toString should be("mov ss, WORD PTR [bp+si]")
      }

      "correctly encode mov ds, WORD PTR [bx+0x99]" in {
        Move(RegisterMemoryLocation[WordSize](BX, 0x99.toByte), DS).encodeByte should be(Hex.lsb("8E 5F 99"))
      }

      "correctly represent mov ds, WORD PTR [bx+153] as a string" in {
        Move(RegisterMemoryLocation[WordSize](BX, 0x99.toByte), DS).toString should be("mov ds, WORD PTR [bx+153]")
      }

      "correctly encode mov al, BYTE PTR [0x0022]" in {
        Move(MemoryAddress[ByteSize](0x0022.toShort), AL).encodeByte should be(Hex.lsb("A0 22 00"))
      }

      "correctly represent mov al, BYTE PTR [34] as a string" in {
        Move(MemoryAddress[ByteSize](0x0022.toShort), AL).toString should be("mov al, BYTE PTR [34]")
      }

      "correctly encode mov ax, WORD PTR [0x6677]" in {
        Move(MemoryAddress[WordSize](0x6677.toShort), AX).encodeByte should be(Hex.lsb("A1 77 66"))
      }

      "correctly represent mov ax, WORD PTR [26231] as a string" in {
        Move(MemoryAddress[WordSize](0x6677.toShort), AX).toString should be("mov ax, WORD PTR [26231]")
      }

      "correctly encode mov ax, WORD PTR ss:[0x6677]" in {
        Move(MemoryAddress[WordSize](0x6677.toShort, SS), AX).encodeByte should be(Hex.lsb("36 A1 77 66"))
      }

      "correctly represent mov ax, WORD PTR ss:[26231] as a string" in {
        Move(MemoryAddress[WordSize](0x6677.toShort, SS), AX).toString should be("mov ax, WORD PTR ss:[26231]")
      }

      "correctly encode mov BYTE PTR [0xDEAF], al" in {
        Move(AL, MemoryAddress[ByteSize](0xDEAF.toShort)).encodeByte should be(Hex.lsb("A2 AF DE"))
      }

      "correctly represent mov BYTE PTR [57007], al as a string" in {
        Move(AL, MemoryAddress[ByteSize](0xDEAF.toShort)).toString should be("mov BYTE PTR [57007], al")
      }

      "correctly encode mov WORD PTR [0x2D], ax" in {
        Move(AX, MemoryAddress[WordSize](0x2D.toByte)).encodeByte should be(Hex.lsb("A3 2D"))
      }

      "correctly represent mov WORD PTR [45], ax as a string" in {
        Move(AX, MemoryAddress[WordSize](0x2D.toByte)).toString should be("mov WORD PTR [45], ax")
      }

      "correctly encode mov dl, 0x12" in {
        Move(0x12.toByte, DL).encodeByte should be(Hex.lsb("B2 12"))
      }

      "correctly represent mov dl, 18 as a string" in {
        Move(0x12.toByte, DL).toString should be("mov dl, 18")
      }

      "correctly encode mov bx, 0x5555" in {
        Move[WordSize](0x5555.toShort, BX).encodeByte should be(Hex.lsb("BB 55 55"))
      }

      "correctly represent mov bx, 21845 as a string" in {
        Move[WordSize](0x5555.toShort, BX).toString should be("mov bx, 21845")
      }

      "correctly encode mov bx, [label]" in {
        val targetLabel = Label.unique
        val move = Move.forLabel(targetLabel, AX)

        val p = Section.text(List[Resource](
          move,
          EncodedBytes(List.fill(1)(0x00.toByte)),
          EncodedBytes(List.fill(1)(0x00.toByte)).label(targetLabel)
        ))

        val app = Raw(p, 0)
        val encodables = app.encodablesForDependencies(Seq(move))
        withClue("Move") { encodables(move).encodeByte should be(Hex.lsb("B8 04 00")) }
      }

      "correctly encode mov esi, 0x78563412" in {
        Move[DoubleWordSize](0x78563412, ESI).encodeByte should be(Hex.lsb("66 BE 12 34 56 78"))
      }

      "correctly represent mov esi, 2018915346 as a string" in {
        Move[DoubleWordSize](0x78563412, ESI).toString should be("mov esi, 2018915346")
      }

      "correctly encode mov BYTE PTR [bp+di], 0x13" in {
        Move(0x13.toByte, RegisterMemoryLocation[ByteSize](BP+DI)).encodeByte should be(Hex.lsb("C6 03 13"))
      }

      "correctly represent mov BYTE PTR [bp+di], 19 as a string" in {
        Move(0x13.toByte, RegisterMemoryLocation[ByteSize](BP+DI)).toString should be("mov BYTE PTR [bp+di], 19")
      }

      "correctly encode mov WORD PTR [bx+0x10], 0x5656" in {
        Move(0x5656.toShort, RegisterMemoryLocation[WordSize](BX, 0x10.toByte)).encodeByte should be(Hex.lsb("C7 47 10 56 56"))
      }

      "correctly represent mov WORD PTR [bx+16], 22102 as a string" in {
        Move(0x5656.toShort, RegisterMemoryLocation[WordSize](BX, 0x10.toByte)).toString should be("mov WORD PTR [bx+16], 22102")
      }

      "correctly encode mov DWORD PTR [eax+ebx*2+0x11111111], 0x99999999" in {
        Move(0x99999999, SIBMemoryLocation[DoubleWordSize](EBX, EAX, 0x11111111, 2)).encodeByte should be(Hex.lsb("67 66 C7 84 58 11 11 11 11 99 99 99 99"))
      }

      "correctly represent mov DWORD PTR [eax+ebx*2+286331153], 2576980377 as a string" in {
        Move(0x99999999, SIBMemoryLocation[DoubleWordSize](EBX, EAX, 0x11111111, 2)).toString should be("mov DWORD PTR [eax+ebx*2+286331153], 2576980377")
      }
    }

    "in protected mode" should {

      import ProcessorMode.Protected._

      "correctly encode mov DWORD PTR [0xDEADBEEF], eax" in {
        Move(EAX, MemoryAddress[DoubleWordSize](0xDEADBEEF)).encodeByte should be(Hex.lsb("A3 EF BE AD DE"))
      }

      "correctly represent mov DWORD PTR [3735928559], eax as a string" in {
        Move(EAX, MemoryAddress[DoubleWordSize](0xDEADBEEF)).toString should be("mov DWORD PTR [3735928559], eax")
      }

      "correctly encode mov eax, DWORD PTR [0xFAFAFAFA]" in {
        Move(MemoryAddress[DoubleWordSize](0xFAFAFAFA), EAX).encodeByte should be(Hex.lsb("A1 FA FA FA FA"))
      }

      "correctly represent mov eax, DWORD PTR [4210752250] as a string" in {
        Move(MemoryAddress[DoubleWordSize](0xFAFAFAFA), EAX).toString should be("mov eax, DWORD PTR [4210752250]")
      }

      "correctly encode mov DWORD PTR [edx], ebp" in {
        Move(EBP, RegisterMemoryLocation[DoubleWordSize](EDX)).encodeByte should be(Hex.lsb("89 2A"))
      }

      "correctly represent mov DWORD PTR [edx], ebp as a string" in {
        Move(EBP, RegisterMemoryLocation[DoubleWordSize](EDX)).toString should be("mov DWORD PTR [edx], ebp")
      }

     "correctly encode mov DWORD PTR [ebp], ebp" in {
        Move(EBP, RegisterMemoryLocation[DoubleWordSize](EBP)).encodeByte should be(Hex.lsb("89 6D 00"))
      }

      "correctly represent mov DWORD PTR [ebp], ebp as a string" in {
        Move(EBP, RegisterMemoryLocation[DoubleWordSize](EBP)).toString should be("mov DWORD PTR [ebp], ebp")
      }

      "correctly encode mov ecx, [label]" in {
        val targetLabel = Label.unique
        val move = Move.forLabel(targetLabel, ECX)

        val p = Section.text(List[Resource](
          EncodedBytes(List.fill(1)(0x00.toByte)),
          move,
          EncodedBytes(List.fill(1)(0x00.toByte)),
          EncodedBytes(List.fill(1)(0x00.toByte)).label(targetLabel)
        ))

        val app = Raw(p, 0x100)
        val encodables = app.encodablesForDependencies(Seq(move))
        withClue("Move") { encodables(move).encodeByte should be(Hex.lsb("B9 07 01 00 00")) }
      }
    }

    "in long mode" should {

      import ProcessorMode.Long._

      "throw an AssertionError for mov bh, r8l" in {
        an[AssertionError] should be thrownBy {
          Move(R8L, BH)
        }
      }

      "throw an AssertionError for mov r8l, bh" in {
        an[AssertionError] should be thrownBy {
          Move(BH, R8L)
        }
      }

      "correctly encode mov bl, r13l" in {
        Move(R13L, BL).encodeByte should be(Hex.lsb("44 88 EB"))
      }

      "correctly represent mov bl, r13l as a string" in {
        Move(R13L, BL).toString should be("mov bl, r13l")
      }

      "correctly encode mov r15l, al" in {
        Move(AL, R15L).encodeByte should be(Hex.lsb("41 88 C7"))
      }

      "correctly represent mov r15l, al as a string" in {
        Move(AL, R15L).toString should be("mov r15l, al")
      }

      "correctly encode mov r10w, ax" in {
        Move(AX, R10W).encodeByte should be(Hex.lsb("66 41 89 C2"))
      }

      "correctly represent mov r10w, ax as a string" in {
        Move(AX, R10W).toString should be("mov r10w, ax")
      }

      "correctly encode mov ecx, r10d" in {
        Move(R10D, ECX).encodeByte should be(Hex.lsb("44 89 D1"))
      }

      "correctly represent mov ecx, r10d as a string" in {
        Move(R10D, ECX).toString should be("mov ecx, r10d")
      }

      "correctly encode mov rcx, r10" in {
        Move(R10, RCX).encodeByte should be(Hex.lsb("4C 89 D1"))
      }

      "correctly represent mov rcx, r10 as a string" in {
        Move(R10, RCX).toString should be("mov rcx, r10")
      }

      "correctly encode mov r13l, BYTE PTR [eax]" in {
        Move(RegisterMemoryLocation[ByteSize](EAX), R13L).encodeByte should be(Hex.lsb("67 44 8A 28"))
      }

      "correctly represent mov r13l, BYTE PTR [eax] as a string" in {
        Move(RegisterMemoryLocation[ByteSize](EAX), R13L).toString should be("mov r13l, BYTE PTR [eax]")
      }

      "correctly encode mov r15w, WORD PTR [eax]" in {
        Move(RegisterMemoryLocation[WordSize](EAX), R15W).encodeByte should be(Hex.lsb("67 66 44 8B 38"))
      }

      "correctly represent mov r15w, WORD PTR [eax] as a string" in {
        Move(RegisterMemoryLocation[WordSize](EAX), R15W).toString should be("mov r15w, WORD PTR [eax]")
      }

      "correctly encode mov ax, WORD PTR [eax]" in {
        Move(RegisterMemoryLocation[WordSize](EAX), AX).encodeByte should be(Hex.lsb("67 66 8B 00"))
      }

      "correctly represent mov ax, WORD PTR [eax] as a string" in {
        Move(RegisterMemoryLocation[WordSize](EAX), AX).toString should be("mov ax, WORD PTR [eax]")
      }


      "correctly encode mov r11w, cs" in {
        Move(CS, R11W).encodeByte should be(Hex.lsb("66 41 8C CB"))
      }

      "correctly represent mov r11w, cs as a string" in {
        Move(CS, R11W).toString should be("mov r11w, cs")
      }

      "correctly encode mov r11, cs" in {
        Move(CS, R11).encodeByte should be(Hex.lsb("49 8C CB"))
      }

      "correctly represent mov r11, cs as a string" in {
        Move(CS, R11).toString should be("mov r11, cs")
      }

      "correctly encode mov gs, r8w" in {
        Move(R8W, GS).encodeByte should be(Hex.lsb("66 41 8E E8"))
      }

      "correctly represent mov gs, r8w as a string" in {
        Move(R8W, GS).toString should be("mov gs, r8w")
      }

      "correctly encode mov gs, r8" in {
        Move(R8, GS).encodeByte should be(Hex.lsb("49 8E E8"))
      }

      "correctly represent mov gs, r8 as a string" in {
        Move(R8, GS).toString should be("mov gs, r8")
      }

      "correctly encode mov rax, QWORD PTR [0xA4A3A2A1F4F3F2F1]" in {
        Move(MemoryAddress[QuadWordSize](0xA4A3A2A1F4F3F2F1L), RAX).encodeByte should be(Hex.lsb("48 A1 F1 F2 F3 F4 A1 A2 A3 A4"))
      }

      "correctly represent mov rax, QWORD PTR [-6583239413802470671] as a string" in {
        Move(MemoryAddress[QuadWordSize](0xA4A3A2A1F4F3F2F1L), RAX).toString should be("mov rax, QWORD PTR [-6583239413802470671]")
      }

      "correctly encode mov QWORD PTR [0xDEADBEEF], rax" in {
        Move(RAX, MemoryAddress[QuadWordSize](0xDEADBEEF)).encodeByte should be(Hex.lsb("67 48 A3 EF BE AD DE"))
      }

      "correctly represent mov QWORD PTR [3735928559], rax as a string" in {
        Move(RAX, MemoryAddress[QuadWordSize](0xDEADBEEF)).toString should be("mov QWORD PTR [3735928559], rax")
      }

      "correctly encode mov edx, 0x12" in {
        Move[DoubleWordSize](0x12, EDX).encodeByte should be(Hex.lsb("BA 12 00 00 00"))
      }

      "correctly encode mov r15l, 0x12" in {
        Move(0x12.toByte, R15L).encodeByte should be(Hex.lsb("41 B7 12"))
      }

      "correctly represent mov r15l, 18 as a string" in {
        Move(0x12.toByte, R15L).toString should be("mov r15l, 18")
      }

      "correctly encode mov r14d, 0x78563412" in {
        Move[DoubleWordSize](0x78563412, R14D).encodeByte should be(Hex.lsb("41 BE 12 34 56 78"))
      }

      "correctly encode mov r11, [label]" in {
        val targetLabel = Label.unique
        val move = Move.forLabel(targetLabel, R11)

        val p = Section.text(List[Resource](
          EncodedBytes(List.fill(2)(0x00.toByte)),
          move,
          EncodedBytes(List.fill(2)(0x00.toByte)),
          EncodedBytes(List.fill(1)(0x00.toByte)).label(targetLabel)
        ))

        val app = Raw(p, 0x10000)
        val encodables = app.encodablesForDependencies(Seq(move))
        withClue("Move") { encodables(move).encodeByte should be(Hex.lsb("49 BB 0E 00 01 00 00 00 00 00")) }
      }

       "correctly encode mov rbx, [label]" in {
        val targetLabel = Label.unique
        val move = Move.forLabel(targetLabel, RBX)

        val p = Section.text(List[Resource](
          EncodedBytes(List.fill(2)(0x00.toByte)),
          EncodedBytes(List.fill(1)(0x00.toByte)).label(targetLabel),
          EncodedBytes(List.fill(2)(0x00.toByte)),
          move
        ))

        val app = Raw(p, 0x3000000)
        val encodables = app.encodablesForDependencies(Seq(move))
        withClue("Move") { encodables(move).encodeByte should be(Hex.lsb("48 BB 02 00 00 03 00 00 00 00")) }
      }

     "correctly represent mov r14d, 2018915346 as a string" in {
        Move[DoubleWordSize](0x78563412, R14D).toString should be("mov r14d, 2018915346")
      }

      "correctly encode mov rax, 0x1122334455667788" in {
        Move[QuadWordSize](0x1122334455667788L, RAX).encodeByte should be(Hex.lsb("48 B8 88 77 66 55 44 33 22 11"))
      }

      "correctly represent mov rax, 1234605616436508552 as a string" in {
        Move[QuadWordSize](0x1122334455667788L, RAX).toString should be("mov rax, 1234605616436508552")
      }

      "correctly encode mov BYTE PTR [rbx], 0x13" in {
        Move(0x13.toByte, RegisterMemoryLocation[ByteSize](RBX)).encodeByte should be(Hex.lsb("C6 03 13"))
      }

      "correctly represent mov BYTE PTR [rbx], 19 as a string" in {
        Move(0x13.toByte, RegisterMemoryLocation[ByteSize](RBX)).toString should be("mov BYTE PTR [rbx], 19")
      }

      "correctly encode mov WORD PTR [rbx+0x10], 0x5656" in {
        Move(0x5656.toShort, RegisterMemoryLocation[WordSize](RBX, 0x10.toByte)).encodeByte should be(Hex.lsb("66 C7 43 10 56 56"))
      }

      "correctly represent mov WORD PTR [rbx+16], 22102 as a string" in {
        Move(0x5656.toShort, RegisterMemoryLocation[WordSize](RBX, 0x10.toByte)).toString should be("mov WORD PTR [rbx+16], 22102")
      }

      "correctly encode mov DWORD PTR [rax+rbx*2+0x11111111], 0x99999999" in {
        Move(0x99999999, SIBMemoryLocation[DoubleWordSize](RBX, RAX, 0x11111111, 2)).encodeByte should be(Hex.lsb("C7 84 58 11 11 11 11 99 99 99 99"))
      }

      "correctly represent mov DWORD PTR [rax+rbx*2+286331153], 2576980377 as a string" in {
        Move(0x99999999, SIBMemoryLocation[DoubleWordSize](RBX, RAX, 0x11111111, 2)).toString should be("mov DWORD PTR [rax+rbx*2+286331153], 2576980377")
      }

      "correctly encode mov QWORD PTR [rax+rbx*2+0x11111111], 0x99999999" in {
        Move(0x99999999L, SIBMemoryLocation[QuadWordSize](RBX, RAX, 0x11111111, 2)).encodeByte should be(Hex.lsb("48 C7 84 58 11 11 11 11 99 99 99 99 00 00 00 00"))
      }

      "correctly represent mov QWORD PTR [rax+rbx*2+286331153], 2576980377 as a string" in {
        Move(0x99999999L, SIBMemoryLocation[QuadWordSize](RBX, RAX, 0x11111111, 2)).toString should be("mov QWORD PTR [rax+rbx*2+286331153], 2576980377")
      }

      "correctly encode mov DWORD PTR [r8+r9*2], ebp" in {
        Move(EBP, SIBMemoryLocation[DoubleWordSize](R9, R8, 0, 2)).encodeByte should be(Hex.lsb("43 89 ac 48 00 00 00 00"))
      }

      "correctly represent mov DWORD PTR [r8+r9*2+0], ebp as a string" in {
        Move(EBP, SIBMemoryLocation[DoubleWordSize](R9, R8, 0, 2)).toString should be("mov DWORD PTR [r8+r9*2+0], ebp")
      }
    }
  }
}