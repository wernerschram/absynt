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

package org.werner.absynt.x86.instructions.branch

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.werner.absynt._
import org.werner.absynt.output.raw.Raw
import org.werner.absynt.resource.Resource
import org.werner.absynt.sections.Section
import org.werner.absynt.x86.ProcessorMode
import org.werner.absynt.x86.operands._
import org.werner.absynt.x86.operands.memoryaccess._
import scala.language.implicitConversions

class CallSuite extends AnyWordSpec with Matchers {
  "a Call instruction" when {
    "in real mode" should {
      import ProcessorMode.Real._

      "correctly encode call 0x10" in {
        Call(wordPointer(0x10.toShort)).encodeByte should be(Hex.lsb("E8 10 00"))
      }
      "correctly represent call 0x10 as a string" in {
        Call(wordPointer(0x10.toShort)).toString should be("call 0x0010")
      }

      "correctly encode call 0x10203040" in {
        Call(doubleWordPointer(0x10203040)).encodeByte should be(Hex.lsb("66 E8 40 30 20 10"))
      }
      "correctly represent call 0x10203040 as a string" in {
        Call(doubleWordPointer(0x10203040)).toString should be("call 0x10203040")
      }

      "correctly encode call ax" in {
        Call(AX).encodeByte should be(Hex.lsb("FF D0"))
      }
      "correctly represent call ax as a string" in {
        Call(AX).toString should be("call ax")
      }

      "correctly encode call FAR DWORD PTR edx" in {
        Call(RegisterMemoryLocation[DoubleWordSize](EDX)).encodeByte should be(Hex.lsb("67 66 FF 12"))
      }
      "correctly represent call FAR DWORD PTR edx as a string" in {
        Call(RegisterMemoryLocation[DoubleWordSize](EDX)).toString should be("call DWORD PTR [edx]")
      }

      "correctly encode call FAR 0x1000:0x2000" in {
        Call.Far(FarPointer[WordSize](0x1000.toShort, 0x2000.toShort)).encodeByte should be(Hex.lsb("9A 00 20 00 10"))
      }
      "correctly represent call FAR 0x1000:0x2000 as a string" in {
        Call.Far(FarPointer[WordSize](0x1000.toShort, 0x2000.toShort)).toString should be("call FAR 0x1000:0x2000")
      }

      "correctly encode call FAR 0x0030:0x200010" in {
        Call.Far(FarPointer[DoubleWordSize](0x30.toShort, 0x200010)).encodeByte should be(Hex.lsb("66 9A 10 00 20 00 30 00"))
      }
      "correctly represent call FAR 0x0030:0x200010 as a string" in {
        Call.Far(FarPointer[DoubleWordSize](0x30.toShort, 0x200010)).toString should be("call FAR 0x0030:0x00200010")
      }

      "correctly encode call FAR WORD PTR [bp+si]" in {
        Call.Far(RegisterMemoryLocation[WordSize](BP + SI)).encodeByte should be(Hex.lsb("FF 1A"))
      }
      "correctly represent call FAR WORD PTR [bp+si] as a string" in {
        Call.Far(RegisterMemoryLocation[WordSize](BP + SI)).toString should be("call FAR WORD PTR [bp+si]")
      }

      "correctly encode call FAR DWORD PTR [bp+si]" in {
        Call.Far(RegisterMemoryLocation[DoubleWordSize](BP + SI)).encodeByte should be(Hex.lsb("66 FF 1A"))
      }
      "correctly represent call FAR DWORD PTR [bp+si] as a string" in {
        Call.Far(RegisterMemoryLocation[DoubleWordSize](BP + SI)).toString should be("call FAR DWORD PTR [bp+si]")
      }

      "Encode a simple program with an indirect backward short call instruction" in {
        val targetLabel = Label.unique
        val call = Call(targetLabel)

        val p = Section.text(List[Resource](
          EncodedBytes.fill(1, 0x00.toByte).label(targetLabel),
          EncodedBytes.fill(1, 0x00.toByte),
          call
        ))

        val app = Raw(p, 0)
        val encodable = app.encodablesForDependencies(call :: Nil)

        withClue("Jump") {
          encodable(call).encodeByte should be(Hex.lsb("E8 FB FF"))
        }
      }

      "Encode a simple program with an indirect forward doubleword sized long call instruction" in {
        val targetLabel = Label.unique
        val call = Call(targetLabel)

        val p = Section.text(List[Resource](
          call,
          EncodedBytes.fill(65536, 0x00.toByte),
          EncodedBytes.fill(1, 0x00.toByte).label(targetLabel)
        ))

        val app = Raw(p, 0)
        val encodable = app.encodablesForDependencies(call :: Nil)

        withClue("Jump") {
          encodable(call).encodeByte should be(Hex.lsb("66 E8 00 00 01 00"))
        }
      }
    }

    "in protected mode" should {
      import ProcessorMode.Protected._

      "correctly encode call 0x10" in {
        Call(wordPointer(0x10.toShort)).encodeByte should be(Hex.lsb("66 E8 10 00"))
      }
      "correctly represent call 0x10 as a string" in {
        Call(wordPointer(0x10.toShort)).toString should be("call 0x0010")
      }

      "correctly encode call 0x10203040" in {
        Call(doubleWordPointer(0x10203040)).encodeByte should be(Hex.lsb("E8 40 30 20 10"))
      }
      "correctly represent call 0x10203040 as a string" in {
        Call(doubleWordPointer(0x10203040)).toString should be("call 0x10203040")
      }

      "correctly encode call ax" in {
        Call(AX).encodeByte should be(Hex.lsb("66 FF D0"))
      }
      "correctly represent call ax as a string" in {
        Call(AX).toString should be("call ax")
      }

      "correctly encode call FAR DWORD PTR edx" in {
        Call(RegisterMemoryLocation[DoubleWordSize](EDX)).encodeByte should be(Hex.lsb("FF 12"))
      }
      "correctly represent call FAR DWORD PTR edx as a string" in {
        Call(RegisterMemoryLocation[DoubleWordSize](EDX)).toString should be("call DWORD PTR [edx]")
      }

      "correctly encode call FAR 0x1000:0x2000" in {
        Call.Far(FarPointer[WordSize](0x1000.toShort, 0x2000.toShort)).encodeByte should be(Hex.lsb("66 9A 00 20 00 10"))
      }
      "correctly represent call FAR 0x1000:0x2000 as a string" in {
        Call.Far(FarPointer[WordSize](0x1000.toShort, 0x2000.toShort)).toString should be("call FAR 0x1000:0x2000")
      }

      "correctly encode call FAR 0x0030:0x200010" in {
        Call.Far(FarPointer[DoubleWordSize](0x30.toShort, 0x200010)).encodeByte should be(Hex.lsb("9A 10 00 20 00 30 00"))
      }
      "correctly represent call FAR 0x0030:0x200010 as a string" in {
        Call.Far(FarPointer[DoubleWordSize](0x30.toShort, 0x200010)).toString should be("call FAR 0x0030:0x00200010")
      }

      "correctly encode call FAR WORD PTR [bp+si]" in {
        Call.Far(RegisterMemoryLocation[WordSize](BP + SI)).encodeByte should be(Hex.lsb("67 66 FF 1A"))
      }
      "correctly represent call FAR WORD PTR [bp+si] as a string" in {
        Call.Far(RegisterMemoryLocation[WordSize](BP + SI)).toString should be("call FAR WORD PTR [bp+si]")
      }

      "correctly encode call FAR DWORD PTR [bp+si]" in {
        Call.Far(RegisterMemoryLocation[DoubleWordSize](BP + SI)).encodeByte should be(Hex.lsb("67 FF 1A"))
      }
      "correctly represent call FAR DWORD PTR [bp+si] as a string" in {
        Call.Far(RegisterMemoryLocation[DoubleWordSize](BP + SI)).toString should be("call FAR DWORD PTR [bp+si]")
      }

      "Encode a simple program with an indirect backward short call instruction" in {
        val targetLabel = Label.unique
        val call = Call(targetLabel)

        val p = Section.text(List[Resource](
          EncodedBytes.fill(1, 0x00.toByte).label(targetLabel),
          EncodedBytes.fill(1, 0x00.toByte),
          call
        ))

        val app = Raw(p, 0)
        val encodable = app.encodablesForDependencies(call :: Nil)

        withClue("Call") {
          encodable(call).encodeByte should be(Hex.lsb("66 E8 FA FF"))
        }
      }

      "Encode a simple program with an indirect forward doubleword sized long call instruction" in {
        val targetLabel = Label.unique
        val call = Call(targetLabel)

        val p = Section.text(List[Resource](
          call,
          EncodedBytes.fill(65536, 0x00.toByte),
          EncodedBytes.fill(1, 0x00.toByte).label(targetLabel)
        ))

        val app = Raw(p, 0)
        val encodable = app.encodablesForDependencies(call :: Nil)

        withClue("Call") {
          encodable(call).encodeByte should be(Hex.lsb("E8 00 00 01 00"))
        }
      }
    }

    "in long mode" should {
      import ProcessorMode.Long._

      "correctly encode call 0x10203040" in {
        Call(doubleWordPointer(0x10203040)).encodeByte should be(Hex.lsb("E8 40 30 20 10"))
      }
      "correctly represent call 0x10203040 as a string" in {
        Call(doubleWordPointer(0x10203040)).toString should be("call 0x10203040")
      }

      "correctly encode call FAR DWORD PTR edx" in {
        Call(RegisterMemoryLocation[QuadWordSize](EDX)).encodeByte should be(Hex.lsb("67 FF 12"))
      }
      "correctly represent call FAR DWORD PTR edx as a string" in {
        Call(RegisterMemoryLocation[QuadWordSize](EDX)).toString should be("call QWORD PTR [edx]")
      }

      "correctly encode call FAR WORD PTR [edx]" in {
        Call.Far(RegisterMemoryLocation[WordSize](EDX)).encodeByte should be(Hex.lsb("67 66 FF 1A"))
      }
      "correctly represent call FAR WORD PTR [edx] as a string" in {
        Call.Far(RegisterMemoryLocation[WordSize](EDX)).toString should be("call FAR WORD PTR [edx]")
      }

      "correctly encode call FAR DWORD PTR [edx]" in {
        Call.Far(RegisterMemoryLocation[DoubleWordSize](EDX)).encodeByte should be(Hex.lsb("67 FF 1A"))
      }
      "correctly represent call FAR DWORD PTR [edx] as a string" in {
        Call.Far(RegisterMemoryLocation[DoubleWordSize](EDX)).toString should be("call FAR DWORD PTR [edx]")
      }

      "correctly encode call FAR QWORD PTR [rdx]" in {
        Call.Far(RegisterMemoryLocation[QuadWordSize](RDX)).encodeByte should be(Hex.lsb("48 FF 1A"))
      }
      "correctly represent call FAR QWORD PTR [rdx] as a string" in {
        Call.Far(RegisterMemoryLocation[QuadWordSize](RDX)).toString should be("call FAR QWORD PTR [rdx]")
      }

      "Encode a simple program with an indirect backward long call instruction" in {
        val targetLabel = Label.unique
        val call = Call(targetLabel)

        val p = Section.text(List[Resource](
          EncodedBytes.fill(1, 0x00.toByte).label(targetLabel),
          EncodedBytes.fill(1, 0x00.toByte),
          call
        ))

        val app = Raw(p, 0)
        val encodable = app.encodablesForDependencies(call :: Nil)

        withClue("Call") {
          encodable(call).encodeByte should be(Hex.lsb("E8 F9 FF FF FF"))
        }
      }

      "Encode a simple program with an indirect forward doubleword sized long call instruction" in {
        val targetLabel = Label.unique
        val call = Call(targetLabel)

        val p = Section.text(List[Resource](
          call,
          EncodedBytes.fill(65536, 0x00.toByte),
          EncodedBytes.fill(1, 0x00.toByte).label(targetLabel)
        ))

        val app = Raw(p, 0)
        val encodable = app.encodablesForDependencies(call :: Nil)

        withClue("Call") {
          encodable(call).encodeByte should be(Hex.lsb("E8 00 00 01 00"))
        }
      }
    }
  }

  "a Ret instruction" when {
    "in real mode" should {
      import ProcessorMode.Real._

      "correctly encode ret" in {
        Return().encodeByte should be(Hex.lsb("C3"))
      }
      "correctly represent ret as a string" in {
        Return().toString should be("ret")
      }

      "correctly encode ret 0x1" in {
        Return(0x1.toShort).encodeByte should be(Hex.lsb("C2 01 00"))
      }

      "correctly represent ret 0x1 as a string" in {
        Return(0x1.toShort).toString should be("ret 1")
      }

      "correctly encode retf" in {
        Return.Far().encodeByte should be(Hex.lsb("CB"))
      }

      "correctly represent retf as a string" in {
        Return.Far().toString should be("retf")
      }

      "correctly encode retf 0x1234" in {
        Return.Far(0x1234.toShort).encodeByte should be(Hex.lsb("CA 34 12"))
      }

      "correctly represent retf 0x1234 as a string" in {
        Return.Far(0x1234.toShort).toString should be("retf 4660")
      }
    }

    "in protected mode" should {

      import ProcessorMode.Protected._

      "correctly encode ret" in {
        Return().encodeByte should be(Hex.lsb("C3"))
      }
      "correctly represent ret as a string" in {
        Return().toString should be("ret")
      }

      "correctly encode ret 0x1" in {
        Return(0x1.toShort).encodeByte should be(Hex.lsb("C2 01 00"))
      }

      "correctly represent ret 0x1 as a string" in {
        Return(0x1.toShort).toString should be("ret 1")
      }

      "correctly encode retf" in {
        Return.Far().encodeByte should be(Hex.lsb("CB"))
      }

      "correctly represent retf as a string" in {
        Return.Far().toString should be("retf")
      }

      "correctly encode retf 0x1234" in {
        Return.Far(0x1234.toShort).encodeByte should be(Hex.lsb("CA 34 12"))
      }

      "correctly represent retf 0x1234 as a string" in {
        Return.Far(0x1234.toShort).toString should be("retf 4660")
      }
    }

    "in long mode" should {
      import ProcessorMode.Long._

      "correctly encode ret" in {
        Return().encodeByte should be(Hex.lsb("C3"))
      }
      "correctly represent ret as a string" in {
        Return().toString should be("ret")
      }

      "correctly encode ret 0x1" in {
        Return(0x1.toShort).encodeByte should be(Hex.lsb("C2 01 00"))
      }

      "correctly represent ret 0x1 as a string" in {
        Return(0x1.toShort).toString should be("ret 1")
      }

      "correctly encode retf" in {
        Return.Far().encodeByte should be(Hex.lsb("CB"))
      }

      "correctly represent retf as a string" in {
        Return.Far().toString should be("retf")
      }

      "correctly encode retf 0x1234" in {
        Return.Far(0x1234.toShort).encodeByte should be(Hex.lsb("CA 34 12"))
      }

      "correctly represent retf 0x1234 as a string" in {
        Return.Far(0x1234.toShort).toString should be("retf 4660")
      }
    }
  }
}