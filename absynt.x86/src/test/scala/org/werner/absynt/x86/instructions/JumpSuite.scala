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

import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.{Matchers, WordSpec}
import org.werner.absynt._
import org.werner.absynt.output.raw.Raw
import org.werner.absynt.resource.Resource
import org.werner.absynt.sections.Section
import org.werner.absynt.x86.ProcessorMode
import org.werner.absynt.x86.operands._
import org.werner.absynt.x86.operands.memoryaccess._
import org.werner.absynt.x86.operations.X86Operation

class JumpSuite extends WordSpec with Matchers {

  "an Jump instruction" when {

    "in real mode" should {

      import ProcessorMode.Real._

      val combinations = Table[String, (NearPointer with ByteWordDoubleSize) => X86Operation, String, String](
        ("Mnemonic", "Instruction",              "Short (0x10)", "Long (0x2030)"),
        ("jmp",      Jump(_),                    "EB 10",        "E9 30 20"),
        ("ja",       JumpIfAbove(_),             "77 10",        "0F 87 30 20"),
        ("jae",      JumpIfAboveOrEqual(_),      "73 10",        "0F 83 30 20"),
        ("jb",       JumpIfBelow(_),             "72 10",        "0F 82 30 20"),
        ("jbe",      JumpIfBelowOrEqual(_),      "76 10",        "0F 86 30 20"),
        ("jc",       JumpIfCarry(_),             "72 10",        "0F 82 30 20"),
        ("je",       JumpIfEqual(_),             "74 10",        "0F 84 30 20"),
        ("jg",       JumpIfGreater(_),           "7F 10",        "0F 8F 30 20"),
        ("jge",      JumpIfGreaterOrEqual(_),    "7D 10",        "0F 8D 30 20"),
        ("jl",       JumpIfLess(_),              "7C 10",        "0F 8C 30 20"),
        ("jle",      JumpIfLessOrEqual(_),       "7E 10",        "0F 8E 30 20"),
        ("jna",      JumpIfNotAbove(_),          "76 10",        "0F 86 30 20"),
        ("jnae",     JumpIfNotAboveOrEqual(_),   "72 10",        "0F 82 30 20"),
        ("jnb",      JumpIfNotBelow(_),          "73 10",        "0F 83 30 20"),
        ("jnbe",     JumpIfNotBelowOrEqual(_),   "77 10",        "0F 87 30 20"),
        ("jnc",      JumpIfNoCarry(_),           "73 10",        "0F 83 30 20"),
        ("jne",      JumpIfNotEqual(_),          "75 10",        "0F 85 30 20"),
        ("jng",      JumpIfNotGreater(_),        "7E 10",        "0F 8E 30 20"),
        ("jnge",     JumpIfNotGreaterOrEqual(_), "7C 10",        "0F 8C 30 20"),
        ("jnl",      JumpIfNotLess(_),           "7D 10",        "0F 8D 30 20"),
        ("jnle",     JumpIfNotLessOrEqual(_),    "7F 10",        "0F 8F 30 20")
      )

      forAll(combinations) {
        (mnemonic: String, operation: (NearPointer with ByteWordDoubleSize) => X86Operation, short: String, long: String) => {
          val shortName = s"$mnemonic 0x10"
          val shortInstruction = operation(shortPointer(0x10.toByte))
          val longName = s"$mnemonic 0x2030"
          val longInstruction = operation(longPointer(0x2030.toShort))

          s"correctly encode $shortName" in { shortInstruction.encodeByte shouldBe Hex.lsb(short) }
          s"correctly represent $shortName as a string" in { shortInstruction.toString shouldBe shortName }
          s"correctly encode $longName" in { longInstruction.encodeByte shouldBe Hex.lsb(long) }
          s"correctly represent $longName as a string" in { longInstruction.toString shouldBe longName }
        }
      }

      "correctly encode jcx 0x10" in { JumpIfCountZero(shortPointer(0x10.toByte)).encodeByte should be(Hex.lsb("E3 10")) }
      "correctly represent jcx 0x10 as a string" in { JumpIfCountZero(shortPointer(0x10.toByte)).toString should be("jcx 0x10") }

      "throw an AssertionError for jmp 0x10203040" in { an[AssertionError] should be thrownBy { Jump(longPointer(0x10203040)).encodeByte } }

      "correctly encode jmp ax" in { Jump(AX).encodeByte should be(Hex.lsb("FF E0")) }
      "correctly represent jmp ax as a string" in { Jump(AX).toString should be("jmp ax") }

      "correctly encode jmp WORD PTR [bp+si]" in { Jump(RegisterMemoryLocation[WordSize](BP+SI)).encodeByte should be(Hex.lsb("FF 22")) }
      "correctly represent jmp WORD PTR [bp+si] as a string" in { Jump(RegisterMemoryLocation[WordSize](BP+SI)).toString should be("jmp WORD PTR [bp+si]") }

      "correctly encode jmp eax" in { Jump(EAX).encodeByte should be(Hex.lsb("66 FF E0")) }
      "correctly represent jmp eax as a string" in { Jump(EAX).toString should be("jmp eax") }

      "correctly encode jmp WORD PTR [eax]" in { Jump(RegisterMemoryLocation[WordSize](EAX)).encodeByte should be(Hex.lsb("67 FF 20")) }
      "correctly represent jmp WORD PTR [eax] as a string" in { Jump(RegisterMemoryLocation[WordSize](EAX)).toString should be("jmp WORD PTR [eax]") }

      "correctly encode jmp DWORD PTR fs:[bx+si]" in {
        Jump(RegisterMemoryLocation.withSegmentOverride[DoubleWordSize](BX+SI, segment = FS)).encodeByte should be(Hex.lsb("64 66 FF 20"))
      }
      "correctly represent jmp DWORD PTR fs:[bx+si] as a string" in {
        Jump(RegisterMemoryLocation.withSegmentOverride[DoubleWordSize](BX+SI, segment = FS)).toString should be("jmp DWORD PTR fs:[bx+si]")
      }

      "correctly encode jmp FAR 0x1000:0x2000" in {
        Jump.Far(FarPointer[WordSize](0x1000.toShort, 0x2000.toShort)).encodeByte should be(Hex.lsb("EA 00 20 00 10"))
      }
      "correctly represent jmp FAR 0x1000:0x2000 as a string" in {
        Jump.Far(FarPointer[WordSize](0x1000.toShort, 0x2000.toShort)).toString should be("jmp FAR 0x1000:0x2000")
      }

      "correctly encode jmp FAR 0x0030:0x200010" in {
        Jump.Far(FarPointer[DoubleWordSize](0x30.toShort, 0x200010)).encodeByte should be(Hex.lsb("66 EA 10 00 20 00 30 00"))
      }
      "correctly represent jmp FAR 0x0030:0x200010 as a string" in {
        Jump.Far(FarPointer[DoubleWordSize](0x30.toShort, 0x200010)).toString should be("jmp FAR 0x0030:0x00200010")
      }

      "correctly encode jmp FAR WORD PTR [bp+si]" in {
        Jump.Far(RegisterMemoryLocation[WordSize](BP+SI)).encodeByte should be(Hex.lsb("FF 2A"))
      }
      "correctly represent jmp FAR WORD PTR [bp+si] as a string" in {
        Jump.Far(RegisterMemoryLocation[WordSize](BP+SI)).toString should be("jmp FAR WORD PTR [bp+si]")
      }

      "correctly encode jmp FAR DWORD PTR [bp+si]" in {
        Jump.Far(RegisterMemoryLocation[DoubleWordSize](BP+SI)).encodeByte should be(Hex.lsb("66 FF 2A"))
      }
      "correctly represent jmp FAR DWORD PTR [bp+si] as a string" in {
        Jump.Far(RegisterMemoryLocation[DoubleWordSize](BP+SI)).toString should be("jmp FAR DWORD PTR [bp+si]")
      }

      "Encode a simple program with an indirect forward short jump instruction" in {
        val targetLabel = Label.unique
        val jump = Jump(targetLabel)

        val p = Section.text(List[Resource](
          jump,
          EncodedBytes(List.fill(1)(0x00.toByte)),
          EncodedBytes(List.fill(1)(0x00.toByte)).label(targetLabel)
        ))

        val app = Raw(p, 0)
        val encodable = app.encodablesForDependencies(jump :: Nil)(jump)
        withClue("Jump") { encodable.encodeByte should be(Hex.lsb("EB 01")) }
      }

      "correctly represent jmp Label as a string" in {
        val targetLabel: Label = "Label"
        Jump(targetLabel).toString should be("jmp Label")
      }

      "Encode a simple program with an indirect forward conditional on count zero short jump instruction" in {
        val targetLabel = Label.unique
        val jump = JumpIfCountZero(targetLabel)

        val p = Section.text(List[Resource](
          jump,
          EncodedBytes(List.fill(1)(0x00.toByte)),
          EncodedBytes(List.fill(1)(0x00.toByte)).label(targetLabel)
        ))

        val app = Raw(p, 0)
        val encodable = app.encodablesForDependencies(jump :: Nil)(jump)

        encodable.size should be(2)

        withClue("Jump") { encodable.encodeByte should be(Hex.lsb("E3 01")) }
      }

      "Encode a simple program with an indirect backward short jump instruction" in {
        val targetLabel = Label.unique
        val jump = Jump(targetLabel)

        val p = Section.text(List[Resource](
          EncodedBytes(List.fill(1)(0x00.toByte)).label(targetLabel),
          EncodedBytes(List.fill(1)(0x00.toByte)),
          jump
        ))

        val app = Raw(p, 0)
        val encodable = app.encodablesForDependencies(jump :: Nil)(jump)

        withClue("Jump") { encodable.encodeByte should be(Hex.lsb("EB FC")) }
      }

      "Encode a simple program with an indirect backward conditional on count zero short jump instruction" in {
        val targetLabel = Label.unique
        val jump = JumpIfCountZero(targetLabel)

        val p = Section.text(List[Resource](
          EncodedBytes(List.fill(1)(0x00.toByte)).label(targetLabel),
          EncodedBytes(List.fill(1)(0x00.toByte)),
          jump
        ))

        val app = Raw(p, 0)
        val encodable = app.encodablesForDependencies(jump :: Nil)(jump)

        withClue("Jump") { encodable.encodeByte should be(Hex.lsb("E3 FC")) }
      }

      "Encode a simple program with an indirect forward long jump instruction" in {
        val targetLabel = Label.unique
        val jump = Jump(targetLabel)

        val p = Section.text(List[Resource](
          jump,
          EncodedBytes(List.fill(256)(0x00.toByte)),
          EncodedBytes(List.fill(1)(0x00.toByte)).label(targetLabel)
        ))

        val app = Raw(p, 0)
        val encodable = app.encodablesForDependencies(jump :: Nil)(jump)

        withClue("Jump") { encodable.encodeByte should be(Hex.lsb("E9 00 01")) }
      }

      "throw an AssertionError for a simple program with an indirect forward conditional on count zero long jump instruction" in {
        val targetLabel = Label.unique
        val jump = JumpIfCountZero(targetLabel)

        val p = Section.text(List[Resource](
          jump,
          EncodedBytes(List.fill(256)(0x00.toByte)),
          EncodedBytes(List.fill(1)(0x00.toByte)).label(targetLabel)
        ))

        val app = Raw(p, 0)

        an[AssertionError] should be thrownBy { app.encodablesForDependencies(jump :: Nil)(jump).encodeByte }
      }

      "Encode a simple program with an indirect backward long jump instruction" in {
        val targetLabel = Label.unique
        val jump = Jump(targetLabel)

        val p = Section.text(List[Resource](
          EncodedBytes(List.fill(1)(0x00.toByte)).label(targetLabel),
          EncodedBytes(List.fill(256)(0x00.toByte)),
          jump
        ))

        val app = Raw(p, 0)
        val encodable = app.encodablesForDependencies(jump :: Nil)(jump)

        withClue("Jump") { encodable.encodeByte should be(Hex.lsb("E9 FC FE")) }
      }

      "Encode a program with two indirect short jump instructions of which one jumps across the other" in {
        val label1 = Label.unique
        val label2 = Label.unique
        val jump1 = Jump(label1)
        val jump2 = Jump(label2)

        val p = Section.text(List[Resource](
          EncodedBytes(List.fill(1)(0x00.toByte)).label(label1),
          jump2,
          EncodedBytes(List.fill(1)(0x00.toByte)),
          EncodedBytes(List.fill(1)(0x00.toByte)).label(label2),
          jump1
        ))

        val app = Raw(p, 0)
        val encodable = app.encodablesForDependencies(jump1 :: jump2 :: Nil)

        withClue("Jump1") { encodable(jump1).encodeByte should be(Hex.lsb("EB F9")) }
        withClue("Jump2") { encodable(jump2).encodeByte should be(Hex.lsb("EB 01")) }
      }

      "Encode a program with two indirect short jump instructions of which one depends on the size of the other for its size" in {
        val label1 = Label.unique
        val label2 = Label.unique
        val jump1 = Jump(label1)
        val jump2 = Jump(label2)

        val p = Section.text(List[Resource](
          EncodedBytes(List.fill(1)(0x00.toByte)).label(label1),
          jump2,
          EncodedBytes(List.fill(122)(0x00.toByte)),
          EncodedBytes(List.fill(1)(0x00.toByte)).label(label2),
          jump1
        ))

        val app = Raw(p, 0)
        val encodable = app.encodablesForDependencies(jump1 :: jump2 :: Nil)

        withClue("Jump1") { encodable(jump1).encodeByte should be(Hex.lsb("EB 80")) }
        withClue("Jump2") { encodable(jump2).encodeByte should be(Hex.lsb("EB 7A")) }
      }

      "Encode a program with two indirect jump instructions that depends on the size of the other for its size where both can be short" in {
        val label1 = Label.unique
        val label2 = Label.unique
        val jump1 = Jump(label1)
        val jump2 = Jump(label2)

        val p = Section.text(List[Resource](
          EncodedBytes(List.fill(1)(0x00.toByte)).label(label1),
          jump2,
          EncodedBytes(List.fill(123)(0x00.toByte)),
          jump1,
          EncodedBytes(List.fill(2)(0x00.toByte)),
          EncodedBytes(List.fill(1)(0x00.toByte)).label(label2)
        ))

        val app = Raw(p, 0)
        val encodable = app.encodablesForDependencies(jump1 :: jump2 :: Nil)

        withClue("Jump1") { encodable(jump1).encodeByte should be(Hex.lsb("EB 80")) }
        withClue("Jump2") { encodable(jump2).encodeByte should be(Hex.lsb("EB 7F")) }
      }

      "Encode a program with two indirect jump instructions that depends on the size of the other for its size where the second forces the first to be long" in {
        val label1 = Label.unique
        val label2 = Label.unique
        val jump1 = Jump(label1)
        val jump2 = Jump(label2)

        val p = Section.text(List[Resource](
          EncodedBytes(List.fill(1)(0x00.toByte)).label(label1),
          jump2,
          EncodedBytes(List.fill(123)(0x00.toByte)),
          jump1,
          EncodedBytes(List.fill(3)(0x00.toByte)),
          EncodedBytes(List.fill(1)(0x00.toByte)).label(label2)
        ))

        val app = Raw(p, 0)
        val encodable = app.encodablesForDependencies(jump1 :: jump2 :: Nil)

        withClue("Jump1") { encodable(jump1).encodeByte should be(Hex.lsb("E9 7E FF")) }
        withClue("Jump2") { encodable(jump2).encodeByte should be(Hex.lsb("E9 81 00")) }
      }

      "Encode a program with two indirect jump instructions that depends on the size of the other for its size where the first forces the second to be long" in {
        val label1 = Label.unique
        val label2 = Label.unique
        val jump1 = Jump(label1)
        val jump2 = Jump(label2)

        val p = Section.text(List[Resource](
          EncodedBytes(List.fill(2)(0x00.toByte)).label(label1),
          jump2,
          EncodedBytes(List.fill(123)(0x00.toByte)),
          jump1,
          EncodedBytes(List.fill(2)(0x00.toByte)),
          EncodedBytes(List.fill(1)(0x00.toByte)).label(label2)
        ))

        val app = Raw(p, 0)
        val encodable = app.encodablesForDependencies(jump1 :: jump2 :: Nil)

        withClue("Jump1") { encodable(jump1).encodeByte should be(Hex.lsb("E9 7D FF")) }
        withClue("Jump2") { encodable(jump2).encodeByte should be(Hex.lsb("E9 80 00")) }
      }

      "Encode a program with three indirect jump instructions that depends on the size of the others for its size where all jumps can be short" in {
        val label1 = Label.unique
        val label2 = Label.unique
        val label3 = Label.unique
        val jump1 = Jump(label1)
        val jump2 = Jump(label2)
        val jump3 = Jump(label3)

        val p = Section.text(List[Resource](
          EncodedBytes(List.fill(1)(0x00.toByte)).label(label1),
          jump2,
          EncodedBytes(List.fill(60)(0x00.toByte)),
          jump3,
          EncodedBytes(List.fill(61)(0x00.toByte)),
          jump1,
          EncodedBytes(List.fill(2)(0x00.toByte)),
          EncodedBytes(List.fill(62)(0x00.toByte)).label(label2),
          EncodedBytes(List.fill(1)(0x00.toByte)).label(label3)
        ))

        val app = Raw(p, 0)
        val encodable = app.encodablesForDependencies(jump1 :: jump2 :: jump3 :: Nil)

        withClue("Jump1") { encodable(jump1).encodeByte should be(Hex.lsb("EB 80")) }
        withClue("Jump2") { encodable(jump2).encodeByte should be(Hex.lsb("EB 7F")) }
        withClue("Jump3") { encodable(jump3).encodeByte should be(Hex.lsb("EB 7F")) }
      }

      "Encode a program with three indirect jump instructions that depends on the size of the others for its size where instruction 3 forces the others to be long" in {
        val label1 = Label.unique
        val label2 = Label.unique
        val label3 = Label.unique
        val jump1 = Jump(label1)
        val jump2 = Jump(label2)
        val jump3 = Jump(label3)

        val p = Section.text(List[Resource](
          EncodedBytes(List.fill(1)(0x00.toByte)).label(label1),
          jump2,
          EncodedBytes(List.fill(60)(0x00.toByte)),
          jump3,
          EncodedBytes(List.fill(61)(0x00.toByte)),
          jump1,
          EncodedBytes(List.fill(2)(0x00.toByte)),
          EncodedBytes(List.fill(63)(0x00.toByte)).label(label2),
          EncodedBytes(List.fill(1)(0x00.toByte)).label(label3)
        ))

        val app = Raw(p, 0)
        val encodable = app.encodablesForDependencies(jump1 :: jump2 :: jump3 :: Nil)

        withClue("Jump1") { encodable(jump1).encodeByte should be(Hex.lsb("E9 7D FF")) }
        withClue("Jump2") { encodable(jump2).encodeByte should be(Hex.lsb("E9 81 00")) }
        withClue("Jump3") { encodable(jump3).encodeByte should be(Hex.lsb("E9 81 00")) }
      }

      "Encode a program with three indirect jump instructions that depends on the size of the others for its size where instruction 1 forces the others to be long" in {
        val label1 = Label.unique
        val label2 = Label.unique
        val label3 = Label.unique
        val jump1 = Jump(label1)
        val jump2 = Jump(label2)
        val jump3 = Jump(label3)

        val p = Section.text(List[Resource](
          EncodedBytes(List.fill(2)(0x00.toByte)).label(label1),
            jump2,
            EncodedBytes(List.fill(60)(0x00.toByte)),
            jump3,
            EncodedBytes(List.fill(61)(0x00.toByte)),
            jump1,
            EncodedBytes(List.fill(2)(0x00.toByte)),
            EncodedBytes(List.fill(62)(0x00.toByte)).label(label2),
            EncodedBytes(List.fill(1)(0x00.toByte)).label(label3)
        ))

        val app = Raw(p, 0)
        val encodable = app.encodablesForDependencies(jump1 :: jump2 :: jump3 :: Nil)

        withClue("Jump1") { encodable(jump1).encodeByte should be(Hex.lsb("E9 7C FF")) }
        withClue("Jump2") { encodable(jump2).encodeByte should be(Hex.lsb("E9 81 00")) }
        withClue("Jump3") { encodable(jump3).encodeByte should be(Hex.lsb("E9 80 00")) }
      }

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

      val combinations = Table[String, (NearPointer with ByteWordDoubleSize) => X86Operation, String, String](
        ("Mnemonic", "Instruction",              "Short (0x10)", "Long (0x20304050)"),
        ("jmp",      Jump(_),                    "EB 10",        "E9 50 40 30 20"),
        ("ja",       JumpIfAbove(_),             "77 10",        "0F 87 50 40 30 20"),
        ("jae",      JumpIfAboveOrEqual(_),      "73 10",        "0F 83 50 40 30 20"),
        ("jb",       JumpIfBelow(_),             "72 10",        "0F 82 50 40 30 20"),
        ("jbe",      JumpIfBelowOrEqual(_),      "76 10",        "0F 86 50 40 30 20"),
        ("jc",       JumpIfCarry(_),             "72 10",        "0F 82 50 40 30 20"),
        ("je",       JumpIfEqual(_),             "74 10",        "0F 84 50 40 30 20"),
        ("jg",       JumpIfGreater(_),           "7F 10",        "0F 8F 50 40 30 20"),
        ("jge",      JumpIfGreaterOrEqual(_),    "7D 10",        "0F 8D 50 40 30 20"),
        ("jl",       JumpIfLess(_),              "7C 10",        "0F 8C 50 40 30 20"),
        ("jle",      JumpIfLessOrEqual(_),       "7E 10",        "0F 8E 50 40 30 20"),
        ("jna",      JumpIfNotAbove(_),          "76 10",        "0F 86 50 40 30 20"),
        ("jnae",     JumpIfNotAboveOrEqual(_),   "72 10",        "0F 82 50 40 30 20"),
        ("jnb",      JumpIfNotBelow(_),          "73 10",        "0F 83 50 40 30 20"),
        ("jnbe",     JumpIfNotBelowOrEqual(_),   "77 10",        "0F 87 50 40 30 20"),
        ("jnc",      JumpIfNoCarry(_),           "73 10",        "0F 83 50 40 30 20"),
        ("jne",      JumpIfNotEqual(_),          "75 10",        "0F 85 50 40 30 20"),
        ("jng",      JumpIfNotGreater(_),        "7E 10",        "0F 8E 50 40 30 20"),
        ("jnge",     JumpIfNotGreaterOrEqual(_), "7C 10",        "0F 8C 50 40 30 20"),
        ("jnl",      JumpIfNotLess(_),           "7D 10",        "0F 8D 50 40 30 20"),
        ("jnle",     JumpIfNotLessOrEqual(_),    "7F 10",        "0F 8F 50 40 30 20")
      )

      forAll(combinations) {
        (mnemonic: String, operation: (NearPointer with ByteWordDoubleSize) => X86Operation, short: String, long: String) => {
          val shortName = s"$mnemonic 0x10"
          val shortInstruction = operation(shortPointer(0x10.toByte))
          val longName = s"$mnemonic 0x20304050"
          val longInstruction = operation(longPointer(0x20304050))

          s"correctly encode $shortName" in { shortInstruction.encodeByte shouldBe Hex.lsb(short) }
          s"correctly represent $shortName as a string" in { shortInstruction.toString shouldBe shortName }
          s"correctly encode $longName" in { longInstruction.encodeByte shouldBe Hex.lsb(long) }
          s"correctly represent $longName as a string" in { longInstruction.toString shouldBe longName }
        }
      }

      "correctly encode jmp si" in {
        Jump(AX).encodeByte should be(Hex.lsb("66 FF E0"))
      }

      "correctly encode jmp DWORD PTR [bp+si]" in {
        Jump(RegisterMemoryLocation[DoubleWordSize](BP+SI)).encodeByte should be(Hex.lsb("67 FF 22"))
      }

      "correctly encode jmp eax" in {
        Jump(EAX).encodeByte should be(Hex.lsb("FF E0"))
      }

      "correctly encode jmp DWORD PTR [eax]" in {
        Jump(RegisterMemoryLocation[DoubleWordSize](EAX)).encodeByte should be(Hex.lsb("FF 20"))
      }

      "correctly encode jmp DWORD PTR fs:[bx+si]" in {
        Jump(RegisterMemoryLocation.withSegmentOverride[DoubleWordSize](BX+SI, segment = FS)).encodeByte should be(Hex.lsb("64 67 FF 20"))
      }

      "correctly encode jmp FAR 0x1000:0x2000" in {
        Jump.Far(FarPointer[WordSize](0x1000.toShort, 0x2000.toShort)).encodeByte should be(Hex.lsb("66 EA 00 20 00 10"))
      }

      "correctly encode jmp FAR 0x30:0x200010" in {
        Jump.Far(FarPointer[DoubleWordSize](0x30.toShort, 0x200010)).encodeByte should be(Hex.lsb("EA 10 00 20 00 30 00"))
      }

      "correctly encode jmp FAR WORD PTR [bp+si]" in {
        Jump.Far(RegisterMemoryLocation[WordSize](BP+SI)).encodeByte should be(Hex.lsb("67 66 FF 2A"))
      }

      "correctly encode jmp FAR DWORD PTR [bp+si]" in {
        Jump.Far(RegisterMemoryLocation[DoubleWordSize](BP+SI)).encodeByte should be(Hex.lsb("67 FF 2A"))
      }

      "Encode a simple program with an indirect backward short jump instruction" in {
        val targetLabel = Label.unique
        val jump = Jump(targetLabel)

        val p = Section.text(List[Resource](
          EncodedBytes(List.fill(1)(0x00.toByte)).label(targetLabel),
          EncodedBytes(List.fill(1)(0x00.toByte)),
          jump
        ))

        val app = Raw(p, 0)
        val encodable = app.encodablesForDependencies(jump :: Nil)

        withClue("Jump") { encodable(jump).encodeByte should be(Hex.lsb("EB FC")) }
      }

      "Encode a simple program with an indirect backward long jump instruction" in {
        val targetLabel = Label.unique
        val jump = Jump(targetLabel)

        val p = Section.text(List[Resource](
          EncodedBytes(List.fill(1)(0x00.toByte)).label(targetLabel),
          EncodedBytes(List.fill(256)(0x00.toByte)),
          jump
        ))

        val app = Raw(p, 0)
        val encodable = app.encodablesForDependencies(jump :: Nil)

        withClue("Jump") { encodable(jump).encodeByte should be(Hex.lsb("E9 FA FE FF FF")) }
      }

      "Encode a simple program with an indirect forward long jump instruction" in {
        val targetLabel = Label.unique
        val jump = Jump(targetLabel)

        val p = Section.text(List[Resource](
          jump,
          EncodedBytes(List.fill(256)(0x00.toByte)),
          EncodedBytes(List.fill(1)(0x00.toByte)).label(targetLabel)
        ))

        val app = Raw(p, 0)
        val encodable = app.encodablesForDependencies(jump :: Nil)

        withClue("Jump") { encodable(jump).encodeByte should be(Hex.lsb("E9 00 01 00 00")) }
      }

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

      val combinations = Table[String, (NearPointer with ByteDoubleSize) => X86Operation, String, String](
        ("Mnemonic", "Instruction",              "Short (0x10)", "Long (0x20304050)"),
        ("jmp",      Jump(_),                    "EB 10",        "E9 50 40 30 20"),
        ("ja",       JumpIfAbove(_),             "77 10",        "0F 87 50 40 30 20"),
        ("jae",      JumpIfAboveOrEqual(_),      "73 10",        "0F 83 50 40 30 20"),
        ("jb",       JumpIfBelow(_),             "72 10",        "0F 82 50 40 30 20"),
        ("jbe",      JumpIfBelowOrEqual(_),      "76 10",        "0F 86 50 40 30 20"),
        ("jc",       JumpIfCarry(_),             "72 10",        "0F 82 50 40 30 20"),
        ("je",       JumpIfEqual(_),             "74 10",        "0F 84 50 40 30 20"),
        ("jg",       JumpIfGreater(_),           "7F 10",        "0F 8F 50 40 30 20"),
        ("jge",      JumpIfGreaterOrEqual(_),    "7D 10",        "0F 8D 50 40 30 20"),
        ("jl",       JumpIfLess(_),              "7C 10",        "0F 8C 50 40 30 20"),
        ("jle",      JumpIfLessOrEqual(_),       "7E 10",        "0F 8E 50 40 30 20"),
        ("jna",      JumpIfNotAbove(_),          "76 10",        "0F 86 50 40 30 20"),
        ("jnae",     JumpIfNotAboveOrEqual(_),   "72 10",        "0F 82 50 40 30 20"),
        ("jnb",      JumpIfNotBelow(_),          "73 10",        "0F 83 50 40 30 20"),
        ("jnbe",     JumpIfNotBelowOrEqual(_),   "77 10",        "0F 87 50 40 30 20"),
        ("jnc",      JumpIfNoCarry(_),           "73 10",        "0F 83 50 40 30 20"),
        ("jne",      JumpIfNotEqual(_),          "75 10",        "0F 85 50 40 30 20"),
        ("jng",      JumpIfNotGreater(_),        "7E 10",        "0F 8E 50 40 30 20"),
        ("jnge",     JumpIfNotGreaterOrEqual(_), "7C 10",        "0F 8C 50 40 30 20"),
        ("jnl",      JumpIfNotLess(_),           "7D 10",        "0F 8D 50 40 30 20"),
        ("jnle",     JumpIfNotLessOrEqual(_),    "7F 10",        "0F 8F 50 40 30 20")
      )

      forAll(combinations) {
        (mnemonic: String, operation: (NearPointer with ByteDoubleSize) => X86Operation, short: String, long: String) => {
          val shortName = s"$mnemonic 0x10"
          val shortInstruction = operation(shortPointer(0x10.toByte))
          val longName = s"$mnemonic 0x20304050"
          val longInstruction = operation(longPointer(0x20304050))

          s"correctly encode $shortName" in { shortInstruction.encodeByte shouldBe Hex.lsb(short) }
          s"correctly represent $shortName as a string" in { shortInstruction.toString shouldBe shortName }
          s"correctly encode $longName" in { longInstruction.encodeByte shouldBe Hex.lsb(long) }
          s"correctly represent $longName as a string" in { longInstruction.toString shouldBe longName }
        }
      }

      "correctly encode jmp DWORD PTR [eax]" in {
        Jump(RegisterMemoryLocation[QuadWordSize](EAX)).encodeByte should be(Hex.lsb("67 FF 20"))
      }

      "correctly encode jmp rax" in {
        Jump(RAX).encodeByte should be(Hex.lsb("FF E0"))
      }

      "correctly encode jmp FAR WORD PTR [edx]" in {
        Jump.Far(RegisterMemoryLocation[WordSize](EDX)).encodeByte should be(Hex.lsb("67 66 FF 2A"))
      }

      "correctly encode jmp FAR DWORD PTR [edx]" in {
        Jump.Far(RegisterMemoryLocation[DoubleWordSize](EDX)).encodeByte should be(Hex.lsb("67 FF 2A"))
      }

      "correctly encode jmp FAR QWORD PTR [rdx]" in {
        Jump.Far(RegisterMemoryLocation[QuadWordSize](RDX)).encodeByte should be(Hex.lsb("48 FF 2A"))
      }

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