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
import org.werner.absynt.Hex
import org.werner.absynt.x86.ProcessorMode
import org.werner.absynt.x86.operands.memoryaccess.DestinationReference
import org.werner.absynt.x86.operands.{ByteSize, DoubleWordSize, WordSize}
import org.scalatest.{Matchers, WordSpec}

class StringSuite extends WordSpec with Matchers {

  "an StoreString instruction" when {
    "in legacy mode" should {

      import ProcessorMode.Legacy._

      "correctly encode stos BYTE PTR [edi], al" in {
        StoreString(AL, DestinationReference[ByteSize](DI)).encodeByte should be(Hex.lsb("AA"))
      }

      "correctly encode REP stos BYTE PTR [edi], al" in {
        StoreString.Repeat(AL, DestinationReference[ByteSize](DI)).encodeByte should be(Hex.lsb("F3 AA"))
      }
    }


    "in real mode" should {

      import ProcessorMode.Real._

      val combinations = Table[String, StringOperation, Byte, Byte](
        ("Mnemonic", "Instruction", "ByteOpcode", "WideOpcode"),
        ("ins", InString, 0x6C.toByte, 0x6D.toByte),
        ("movs", MoveString, 0xA4.toByte, 0xA5.toByte),
        ("outs", OutString, 0x6E.toByte, 0x6F.toByte),
        ("lods", LoadString, 0xAC.toByte, 0xAD.toByte),
        ("stos", StoreString, 0xAA.toByte, 0xAB.toByte),
      )
      forAll(combinations) {
        (mnemonic, instruction, byteOpcode, wideOpcode) => {
          val byteName = s"$mnemonic BYTE PTR [di], al"
          val wideName = s"$mnemonic WORD PTR [di], ax"

          s"correctly encode ins $byteName" in {
            instruction(AL, DestinationReference[ByteSize](DI)).encodeByte should be(Seq(byteOpcode))
          }
          s"correctly represent $byteName as a string" in {
            instruction(AL, DestinationReference[ByteSize](DI)).toString should be(byteName)
          }

          s"correctly encode ins $wideName" in {
            instruction(AX, DestinationReference[WordSize](DI)).encodeByte should be(Seq(wideOpcode))
          }
          s"correctly represent $wideName as a string" in {
            instruction(AX, DestinationReference[WordSize](DI)).toString should be(wideName)
          }

          s"correctly encode REP $byteName" in {
            instruction.Repeat(AL, DestinationReference[ByteSize](DI)).encodeByte should be(Seq(0xF3.toByte, byteOpcode))
          }
          s"correctly represent rep $byteName as a string" in {
            instruction.Repeat(AL, DestinationReference[ByteSize](DI)).toString should be(s"rep $byteName")
          }

          s"correctly encode REP $wideName" in {
            instruction.Repeat(AX, DestinationReference[WordSize](DI)).encodeByte should be(Seq(0xF3.toByte, wideOpcode))
          }
          s"correctly represent rep $wideName as a string" in {
            instruction.Repeat(AX, DestinationReference[WordSize](DI)).toString should be(s"rep $wideName")
          }
        }
      }

      val conditionOombinations = Table[String, StringConditionOperation, Byte, Byte](
        ("Mnemonic", "Instruction", "ByteOpcode", "WideOpcode"),
        ("cmps", CompareString, 0xA6.toByte, 0xA7.toByte),
        ("scas", ScanString, 0xAE.toByte, 0xAF.toByte),
      )
      forAll(conditionOombinations) {
        (mnemonic, instruction, byteOpcode, wideOpcode) => {
          val byteName = s"$mnemonic BYTE PTR [di], al"
          val wideName = s"$mnemonic WORD PTR [di], ax"

          s"correctly encode ins $byteName" in {
            instruction(AL, DestinationReference[ByteSize](DI)).encodeByte should be(Seq(byteOpcode))
          }
          s"correctly represent $byteName as a string" in {
            instruction(AL, DestinationReference[ByteSize](DI)).toString should be(byteName)
          }

          s"correctly encode ins $wideName" in {
            instruction(AX, DestinationReference[WordSize](DI)).encodeByte should be(Seq(wideOpcode))
          }
          s"correctly represent $wideName as a string" in {
            instruction(AX, DestinationReference[WordSize](DI)).toString should be(wideName)
          }

          s"correctly encode repe $byteName" in {
            instruction.RepeatEqual(AL, DestinationReference[ByteSize](DI)).encodeByte should be(Seq(0xF3.toByte, byteOpcode))
          }
          s"correctly represent repe $byteName as a string" in {
            instruction.RepeatEqual(AL, DestinationReference[ByteSize](DI)).toString should be(s"repe $byteName")
          }

          s"correctly encode repe $wideName" in {
            instruction.RepeatEqual(AX, DestinationReference[WordSize](DI)).encodeByte should be(Seq(0xF3.toByte, wideOpcode))
          }
          s"correctly represent repe $wideName as a string" in {
            instruction.RepeatEqual(AX, DestinationReference[WordSize](DI)).toString should be(s"repe $wideName")
          }

          s"correctly encode repne $byteName" in {
            instruction.RepeatNotEqual(AL, DestinationReference[ByteSize](DI)).encodeByte should be(Seq(0xF2.toByte, byteOpcode))
          }
          s"correctly represent repne $byteName as a string" in {
            instruction.RepeatNotEqual(AL, DestinationReference[ByteSize](DI)).toString should be(s"repne $byteName")
          }

          s"correctly encode repne $wideName" in {
            instruction.RepeatNotEqual(AX, DestinationReference[WordSize](DI)).encodeByte should be(Seq(0xF2.toByte, wideOpcode))
          }
          s"correctly represent repne $wideName as a string" in {
            instruction.RepeatNotEqual(AX, DestinationReference[WordSize](DI)).toString should be(s"repne $wideName")
          }
        }
      }
    }

    "in protected mode" should {

      import ProcessorMode.Protected._

      val combinations = Table[String, StringOperation, Byte, Byte](
        ("Mnemonic", "Instruction", "ByteOpcode", "WideOpcode"),
        ("ins", InString, 0x6C.toByte, 0x6D.toByte),
        ("movs", MoveString, 0xA4.toByte, 0xA5.toByte),
        ("outs", OutString, 0x6E.toByte, 0x6F.toByte),
        ("lods", LoadString, 0xAC.toByte, 0xAD.toByte),
        ("stos", StoreString, 0xAA.toByte, 0xAB.toByte),
      )
      forAll(combinations) {
        (mnemonic, instruction, byteOpcode, wideOpcode) => {
          val byteName = s"$mnemonic BYTE PTR [edi], al"
          val wideName = s"$mnemonic DWORD PTR [edi], eax"

          s"correctly encode ins $byteName" in {
            instruction(AL, DestinationReference[ByteSize](EDI)).encodeByte should be(Seq(byteOpcode))
          }
          s"correctly represent $byteName as a string" in {
            instruction(AL, DestinationReference[ByteSize](EDI)).toString should be(byteName)
          }

          s"correctly encode ins $wideName" in {
            instruction(EAX, DestinationReference[DoubleWordSize](EDI)).encodeByte should be(Seq(wideOpcode))
          }
          s"correctly represent $wideName as a string" in {
            instruction(EAX, DestinationReference[DoubleWordSize](EDI)).toString should be(wideName)
          }

          s"correctly encode REP $byteName" in {
            instruction.Repeat(AL, DestinationReference[ByteSize](EDI)).encodeByte should be(Seq(0xF3.toByte, byteOpcode))
          }
          s"correctly represent rep $byteName as a string" in {
            instruction.Repeat(AL, DestinationReference[ByteSize](EDI)).toString should be(s"rep $byteName")
          }

          s"correctly encode REP $wideName" in {
            instruction.Repeat(EAX, DestinationReference[DoubleWordSize](EDI)).encodeByte should be(Seq(0xF3.toByte, wideOpcode))
          }
          s"correctly represent rep $wideName as a string" in {
            instruction.Repeat(EAX, DestinationReference[DoubleWordSize](EDI)).toString should be(s"rep $wideName")
          }
        }
      }

      val conditionOombinations = Table[String, StringConditionOperation, Byte, Byte](
        ("Mnemonic", "Instruction", "ByteOpcode", "WideOpcode"),
        ("cmps", CompareString, 0xA6.toByte, 0xA7.toByte),
        ("scas", ScanString, 0xAE.toByte, 0xAF.toByte),
      )
      forAll(conditionOombinations) {
        (mnemonic, instruction, byteOpcode, wideOpcode) => {
          val byteName = s"$mnemonic BYTE PTR [edi], al"
          val wideName = s"$mnemonic DWORD PTR [edi], eax"

          s"correctly encode ins $byteName" in {
            instruction(AL, DestinationReference[ByteSize](EDI)).encodeByte should be(Seq(byteOpcode))
          }
          s"correctly represent $byteName as a string" in {
            instruction(AL, DestinationReference[ByteSize](EDI)).toString should be(byteName)
          }

          s"correctly encode ins $wideName" in {
            instruction(EAX, DestinationReference[DoubleWordSize](EDI)).encodeByte should be(Seq(wideOpcode))
          }
          s"correctly represent $wideName as a string" in {
            instruction(EAX, DestinationReference[DoubleWordSize](EDI)).toString should be(wideName)
          }

          s"correctly encode repe $byteName" in {
            instruction.RepeatEqual(AL, DestinationReference[ByteSize](EDI)).encodeByte should be(Seq(0xF3.toByte, byteOpcode))
          }
          s"correctly represent repe $byteName as a string" in {
            instruction.RepeatEqual(AL, DestinationReference[ByteSize](EDI)).toString should be(s"repe $byteName")
          }

          s"correctly encode repe $wideName" in {
            instruction.RepeatEqual(EAX, DestinationReference[DoubleWordSize](EDI)).encodeByte should be(Seq(0xF3.toByte, wideOpcode))
          }
          s"correctly represent repe $wideName as a string" in {
            instruction.RepeatEqual(EAX, DestinationReference[DoubleWordSize](EDI)).toString should be(s"repe $wideName")
          }

          s"correctly encode repne $byteName" in {
            instruction.RepeatNotEqual(AL, DestinationReference[ByteSize](EDI)).encodeByte should be(Seq(0xF2.toByte, byteOpcode))
          }
          s"correctly represent repne $byteName as a string" in {
            instruction.RepeatNotEqual(AL, DestinationReference[ByteSize](EDI)).toString should be(s"repne $byteName")
          }

          s"correctly encode repne $wideName" in {
            instruction.RepeatNotEqual(EAX, DestinationReference[DoubleWordSize](EDI)).encodeByte should be(Seq(0xF2.toByte, wideOpcode))
          }
          s"correctly represent repne $wideName as a string" in {
            instruction.RepeatNotEqual(EAX, DestinationReference[DoubleWordSize](EDI)).toString should be(s"repne $wideName")
          }
        }
      }
    }

    "in long mode" should {

      import ProcessorMode.Long._

      val combinations = Table[String, StringOperation, Byte, Byte](
        ("Mnemonic", "Instruction", "ByteOpcode", "WideOpcode"),
        ("ins", InString, 0x6C.toByte, 0x6D.toByte),
        ("movs", MoveString, 0xA4.toByte, 0xA5.toByte),
        ("outs", OutString, 0x6E.toByte, 0x6F.toByte),
        ("lods", LoadString, 0xAC.toByte, 0xAD.toByte),
        ("stos", StoreString, 0xAA.toByte, 0xAB.toByte),
      )
      forAll(combinations) {
        (mnemonic, instruction, byteOpcode, wideOpcode) => {
          val byteName = s"$mnemonic BYTE PTR [edi], al"
          val wideName = s"$mnemonic DWORD PTR [edi], eax"

          s"correctly encode ins $byteName" in {
            instruction(AL, DestinationReference[ByteSize](EDI)).encodeByte should be(Seq(byteOpcode))
          }
          s"correctly represent $byteName as a string" in {
            instruction(AL, DestinationReference[ByteSize](EDI)).toString should be(byteName)
          }

          s"correctly encode ins $wideName" in {
            instruction(EAX, DestinationReference[DoubleWordSize](EDI)).encodeByte should be(Seq(wideOpcode))
          }
          s"correctly represent $wideName as a string" in {
            instruction(EAX, DestinationReference[DoubleWordSize](EDI)).toString should be(wideName)
          }

          s"correctly encode REP $byteName" in {
            instruction.Repeat(AL, DestinationReference[ByteSize](EDI)).encodeByte should be(Seq(0xF3.toByte, byteOpcode))
          }
          s"correctly represent rep $byteName as a string" in {
            instruction.Repeat(AL, DestinationReference[ByteSize](EDI)).toString should be(s"rep $byteName")
          }

          s"correctly encode REP $wideName" in {
            instruction.Repeat(EAX, DestinationReference[DoubleWordSize](EDI)).encodeByte should be(Seq(0xF3.toByte, wideOpcode))
          }
          s"correctly represent rep $wideName as a string" in {
            instruction.Repeat(EAX, DestinationReference[DoubleWordSize](EDI)).toString should be(s"rep $wideName")
          }
        }
      }

      val conditionOombinations = Table[String, StringConditionOperation, Byte, Byte](
        ("Mnemonic", "Instruction", "ByteOpcode", "WideOpcode"),
        ("cmps", CompareString, 0xA6.toByte, 0xA7.toByte),
        ("scas", ScanString, 0xAE.toByte, 0xAF.toByte),
      )
      forAll(conditionOombinations) {
        (mnemonic, instruction, byteOpcode, wideOpcode) => {
          val byteName = s"$mnemonic BYTE PTR [edi], al"
          val wideName = s"$mnemonic DWORD PTR [edi], eax"

          s"correctly encode ins $byteName" in {
            instruction(AL, DestinationReference[ByteSize](EDI)).encodeByte should be(Seq(byteOpcode))
          }
          s"correctly represent $byteName as a string" in {
            instruction(AL, DestinationReference[ByteSize](EDI)).toString should be(byteName)
          }

          s"correctly encode ins $wideName" in {
            instruction(EAX, DestinationReference[DoubleWordSize](EDI)).encodeByte should be(Seq(wideOpcode))
          }
          s"correctly represent $wideName as a string" in {
            instruction(EAX, DestinationReference[DoubleWordSize](EDI)).toString should be(wideName)
          }

          s"correctly encode repe $byteName" in {
            instruction.RepeatEqual(AL, DestinationReference[ByteSize](EDI)).encodeByte should be(Seq(0xF3.toByte, byteOpcode))
          }
          s"correctly represent repe $byteName as a string" in {
            instruction.RepeatEqual(AL, DestinationReference[ByteSize](EDI)).toString should be(s"repe $byteName")
          }

          s"correctly encode repe $wideName" in {
            instruction.RepeatEqual(EAX, DestinationReference[DoubleWordSize](EDI)).encodeByte should be(Seq(0xF3.toByte, wideOpcode))
          }
          s"correctly represent repe $wideName as a string" in {
            instruction.RepeatEqual(EAX, DestinationReference[DoubleWordSize](EDI)).toString should be(s"repe $wideName")
          }

          s"correctly encode repne $byteName" in {
            instruction.RepeatNotEqual(AL, DestinationReference[ByteSize](EDI)).encodeByte should be(Seq(0xF2.toByte, byteOpcode))
          }
          s"correctly represent repne $byteName as a string" in {
            instruction.RepeatNotEqual(AL, DestinationReference[ByteSize](EDI)).toString should be(s"repne $byteName")
          }

          s"correctly encode repne $wideName" in {
            instruction.RepeatNotEqual(EAX, DestinationReference[DoubleWordSize](EDI)).encodeByte should be(Seq(0xF2.toByte, wideOpcode))
          }
          s"correctly represent repne $wideName as a string" in {
            instruction.RepeatNotEqual(EAX, DestinationReference[DoubleWordSize](EDI)).toString should be(s"repne $wideName")
          }
        }
      }

    }
  }
}