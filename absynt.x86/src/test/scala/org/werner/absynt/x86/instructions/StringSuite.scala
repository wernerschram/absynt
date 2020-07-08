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
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class StringSuite extends AnyWordSpec with Matchers {

  "an InString instruction" when {
    "in real mode" when {
      import ProcessorMode.Real._

      "not repeated" should {


        s"correctly encode ins BYTE PTR [di], dx" in {
          InString(DX, Pointer.Destination.word[ByteSize](DI)).encodeByte should be(Hex.lsb("6C"))
        }
        s"correctly represent ins BYTE PTR [di], dx as a string" in {
          InString(DX, Pointer.Destination.word[ByteSize](DI)).toString should be("ins BYTE PTR [di], dx")
        }

        s"correctly encode ins BYTE PTR [edi], dx" in {
          InString(DX, Pointer.Destination.doubleWord[ByteSize](EDI)).encodeByte should be(Hex.lsb("67 6C"))
        }
        s"correctly represent ins BYTE PTR [edi], dx as a string" in {
          InString(DX, Pointer.Destination.doubleWord[ByteSize](EDI)).toString should be("ins BYTE PTR [edi], dx")
        }

        s"correctly encode ins WORD PTR [di], dx" in {
          InString(DX, Pointer.Destination.word[WordSize](DI)).encodeByte should be(Hex.lsb("6D"))
        }
        s"correctly represent ins WORD PTR [di], dx as a string" in {
          InString(DX, Pointer.Destination.word[WordSize](DI)).toString should be("ins WORD PTR [di], dx")
        }

        s"correctly encode ins DWORD PTR [di], dx" in {
          InString(DX, Pointer.Destination.word[DoubleWordSize](DI)).encodeByte should be(Hex.lsb("66 6D"))
        }
        s"correctly represent ins DWORD PTR [di], dx as a string" in {
          InString(DX, Pointer.Destination.word[DoubleWordSize](DI)).toString should be("ins DWORD PTR [di], dx")
        }
      }

      "when repeated" should {
        s"correctly encode rep ins BYTE PTR [di], dx" in {
          InString.Repeat(DX, Pointer.Destination.word[ByteSize](DI)).encodeByte should be(Hex.lsb("F3 6C"))
        }
        s"correctly represent rep ins BYTE PTR [di], dx as a string" in {
          InString.Repeat(DX, Pointer.Destination.word[ByteSize](DI)).toString should be("rep ins BYTE PTR [di], dx")
        }

        s"correctly encode rep ins WORD PTR [di], dx" in {
          InString.Repeat(DX, Pointer.Destination.word[WordSize](DI)).encodeByte should be(Hex.lsb("F3 6D"))
        }
        s"correctly represent rep ins WORD PTR [di], dx as a string" in {
          InString.Repeat(DX, Pointer.Destination.word[WordSize](DI)).toString should be("rep ins WORD PTR [di], dx")
        }

        s"correctly encode rep ins DWORD PTR [di], dx" in {
          InString.Repeat(DX, Pointer.Destination.word[DoubleWordSize](DI)).encodeByte should be(Hex.lsb("F3 66 6D"))
        }
        s"correctly represent rep ins DWORD PTR [di], dx as a string" in {
          InString.Repeat(DX, Pointer.Destination.word[DoubleWordSize](DI)).toString should be("rep ins DWORD PTR [di], dx")
        }
      }
    }

    "in protected mode" when {
      import ProcessorMode.Protected._

      "not repeated" should {

        s"correctly encode ins WORD PTR [edi], dx" in {
          InString(DX, Pointer.Destination.doubleWord[WordSize](EDI)).encodeByte should be(Hex.lsb("66 6D"))
        }
        s"correctly represent ins WORD PTR [edi], dx as a string" in {
          InString(DX, Pointer.Destination.doubleWord[WordSize](EDI)).toString should be("ins WORD PTR [edi], dx")
        }

        s"correctly encode ins DWORD PTR [edi], dx" in {
          InString(DX, Pointer.Destination.doubleWord[DoubleWordSize](EDI)).encodeByte should be(Hex.lsb("6D"))
        }
        s"correctly represent ins DWORD PTR [edi], dx as a string" in {
          InString(DX, Pointer.Destination.doubleWord[DoubleWordSize](EDI)).toString should be("ins DWORD PTR [edi], dx")
        }
      }

      "when repeated" should {
        s"correctly encode rep ins BYTE PTR [edi], dx" in {
          InString.Repeat(DX, Pointer.Destination.doubleWord[ByteSize](EDI)).encodeByte should be(Hex.lsb("F3 6C"))
        }
        s"correctly represent rep ins BYTE PTR [edi], dx as a string" in {
          InString.Repeat(DX, Pointer.Destination.doubleWord[ByteSize](EDI)).toString should be("rep ins BYTE PTR [edi], dx")
        }

        s"correctly encode rep ins WORD PTR [edi], dx" in {
          InString.Repeat(DX, Pointer.Destination.doubleWord[WordSize](EDI)).encodeByte should be(Hex.lsb("F3 66 6D"))
        }
        s"correctly represent rep ins WORD PTR [edi], dx as a string" in {
          InString.Repeat(DX, Pointer.Destination.doubleWord[WordSize](EDI)).toString should be("rep ins WORD PTR [edi], dx")
        }

        s"correctly encode rep ins DWORD PTR [edi], dx" in {
          InString.Repeat(DX, Pointer.Destination.doubleWord[DoubleWordSize](EDI)).encodeByte should be(Hex.lsb("F3 6D"))
        }
        s"correctly represent rep ins DWORD PTR [edi], dx as a string" in {
          InString.Repeat(DX, Pointer.Destination.doubleWord[DoubleWordSize](EDI)).toString should be("rep ins DWORD PTR [edi], dx")
        }
      }
    }

    "in long mode" when {
      import ProcessorMode.Long._

      "not repeated" should {

        s"correctly encode ins WORD PTR [rdi], dx" in {
          InString(DX, Pointer.Destination.quadWord[WordSize](RDI)).encodeByte should be(Hex.lsb("66 6D"))
        }
        s"correctly represent ins WORD PTR [rdi], dx as a string" in {
          InString(DX, Pointer.Destination.quadWord[WordSize](RDI)).toString should be("ins WORD PTR [rdi], dx")
        }

        s"correctly encode ins DWORD PTR [rdi], dx" in {
          InString(DX, Pointer.Destination.quadWord[DoubleWordSize](RDI)).encodeByte should be(Hex.lsb("6D"))
        }
        s"correctly represent ins DWORD PTR [rdi], dx as a string" in {
          InString(DX, Pointer.Destination.quadWord[DoubleWordSize](RDI)).toString should be("ins DWORD PTR [rdi], dx")
        }

        s"correctly encode ins QWORD PTR [rdi], dx" in {
          InString(DX, Pointer.Destination.quadWord[QuadWordSize](RDI)).encodeByte should be(Hex.lsb("48 6D"))
        }
        s"correctly represent ins QWORD PTR [rdi], dx as a string" in {
          InString(DX, Pointer.Destination.quadWord[QuadWordSize](RDI)).toString should be("ins QWORD PTR [rdi], dx")
        }

        s"correctly encode ins QWORD PTR [edi], dx" in {
          InString(DX, Pointer.Destination.doubleWord[QuadWordSize](EDI)).encodeByte should be(Hex.lsb("67 48 6D"))
        }
        s"correctly represent ins QWORD PTR [edi], dx as a string" in {
          InString(DX, Pointer.Destination.doubleWord[QuadWordSize](EDI)).toString should be("ins QWORD PTR [edi], dx")
        }
      }
    }
  }

  "an MoveString instruction" when {
    "in legacy mode" should {

      import ProcessorMode.Legacy._

      "correctly encode stos BYTE PTR [edi], al" in {
        StoreString(AL, Pointer.Destination.word[ByteSize](DI)).encodeByte should be(Hex.lsb("AA"))
      }

      "correctly encode REP stos BYTE PTR [edi], al" in {
        StoreString.Repeat(AL, Pointer.Destination.word[ByteSize](DI)).encodeByte should be(Hex.lsb("F3 AA"))
      }
    }


    "in real mode" when {
      import ProcessorMode.Real._

      "not repeated" should {


        s"correctly encode movs BYTE PTR [di], BYTE PTR [si]" in {
          MoveString(Pointer.Source.word[ByteSize](SI), Pointer.Destination.word[ByteSize](DI)).encodeByte should be(Hex.lsb("A4"))
        }
        s"correctly represent movs BYTE PTR [di], BYTE PTR [si] as a string" in {
          MoveString(Pointer.Source.word[ByteSize](SI), Pointer.Destination.word[ByteSize](DI)).toString should be("movs BYTE PTR [di], BYTE PTR [si]")
        }

        s"correctly encode movs BYTE PTR [edi], BYTE PTR [esi]" in {
          MoveString(Pointer.Source.doubleWord[ByteSize](ESI), Pointer.Destination.doubleWord[ByteSize](EDI)).encodeByte should be(Hex.lsb("67 A4"))
        }
        s"correctly represent movs BYTE PTR [edi], BYTE PTR [esi] as a string" in {
          MoveString(Pointer.Source.doubleWord[ByteSize](ESI), Pointer.Destination.doubleWord[ByteSize](EDI)).toString should be("movs BYTE PTR [edi], BYTE PTR [esi]")
        }

        s"correctly encode movs WORD PTR [di], WORD PTR [si]" in {
          MoveString(Pointer.Source.word[WordSize](SI), Pointer.Destination.word[WordSize](DI)).encodeByte should be(Hex.lsb("A5"))
        }
        s"correctly represent movs WORD PTR [di], WORD PTR [si] as a string" in {
          MoveString(Pointer.Source.word[WordSize](SI), Pointer.Destination.word[WordSize](DI)).toString should be("movs WORD PTR [di], WORD PTR [si]")
        }

        s"correctly encode movs DWORD PTR [di], DWORD PTR [si]" in {
          MoveString(Pointer.Source.word[DoubleWordSize](SI), Pointer.Destination.word[DoubleWordSize](DI)).encodeByte should be(Hex.lsb("66 A5"))
        }
        s"correctly represent movs DWORD PTR [di], DWORD PTR [si] as a string" in {
          MoveString(Pointer.Source.word[DoubleWordSize](SI), Pointer.Destination.word[DoubleWordSize](DI)).toString should be("movs DWORD PTR [di], DWORD PTR [si]")
        }
      }

      "when repeated" should {
        s"correctly encode rep movs BYTE PTR [di], BYTE PTR [si]" in {
          MoveString.Repeat(Pointer.Source.word[ByteSize](SI), Pointer.Destination.word[ByteSize](DI)).encodeByte should be(Hex.lsb("F3 A4"))
        }
        s"correctly represent rep movs BYTE PTR [di], BYTE PTR [si] as a string" in {
          MoveString.Repeat(Pointer.Source.word[ByteSize](SI), Pointer.Destination.word[ByteSize](DI)).toString should be("rep movs BYTE PTR [di], BYTE PTR [si]")
        }

        s"correctly encode rep movs WORD PTR [di], WORD PTR [si]" in {
          MoveString.Repeat(Pointer.Source.word[WordSize](SI), Pointer.Destination.word[WordSize](DI)).encodeByte should be(Hex.lsb("F3 A5"))
        }
        s"correctly represent rep movs WORD PTR [di], WORD PTR [si] as a string" in {
          MoveString.Repeat(Pointer.Source.word[WordSize](SI), Pointer.Destination.word[WordSize](DI)).toString should be("rep movs WORD PTR [di], WORD PTR [si]")
        }

        s"correctly encode rep movs DWORD PTR [di], DWORD PTR [si]" in {
          MoveString.Repeat(Pointer.Source.word[DoubleWordSize](SI), Pointer.Destination.word[DoubleWordSize](DI)).encodeByte should be(Hex.lsb("F3 66 A5"))
        }
        s"correctly represent rep movs DWORD PTR [di], DWORD PTR [si] as a string" in {
          MoveString.Repeat(Pointer.Source.word[DoubleWordSize](SI), Pointer.Destination.word[DoubleWordSize](DI)).toString should be("rep movs DWORD PTR [di], DWORD PTR [si]")
        }
      }
    }

    "in protected mode" when {
      import ProcessorMode.Protected._

      "not repeated" should {
        s"correctly encode movs WORD PTR [edi], WORD PTR [esi]" in {
          MoveString(Pointer.Source.doubleWord[WordSize](ESI), Pointer.Destination.doubleWord[WordSize](EDI)).encodeByte should be(Hex.lsb("66 A5"))
        }
        s"correctly represent movs WORD PTR [edi], WORD PTR [esi] as a string" in {
          MoveString(Pointer.Source.doubleWord[WordSize](ESI), Pointer.Destination.doubleWord[WordSize](EDI)).toString should be("movs WORD PTR [edi], WORD PTR [esi]")
        }

        s"correctly encode movs DWORD PTR [edi], DWORD PTR [esi]" in {
          MoveString(Pointer.Source.doubleWord[DoubleWordSize](ESI), Pointer.Destination.doubleWord[DoubleWordSize](EDI)).encodeByte should be(Hex.lsb("A5"))
        }
        s"correctly represent movs DWORD PTR [edi], DWORD PTR [esi] as a string" in {
          MoveString(Pointer.Source.doubleWord[DoubleWordSize](ESI), Pointer.Destination.doubleWord[DoubleWordSize](EDI)).toString should be("movs DWORD PTR [edi], DWORD PTR [esi]")
        }
      }

      "when repeated" should {
        s"correctly encode rep movs WORD PTR [edi], WORD PTR [esi]" in {
          MoveString.Repeat(Pointer.Source.doubleWord[WordSize](ESI), Pointer.Destination.doubleWord[WordSize](EDI)).encodeByte should be(Hex.lsb("F3 66 A5"))
        }
        s"correctly represent rep movs WORD PTR [edi], WORD PTR [esi] as a string" in {
          MoveString.Repeat(Pointer.Source.doubleWord[WordSize](ESI), Pointer.Destination.doubleWord[WordSize](EDI)).toString should be("rep movs WORD PTR [edi], WORD PTR [esi]")
        }

        s"correctly encode rep movs DWORD PTR [edi], DWORD PTR [esi]" in {
          MoveString.Repeat(Pointer.Source.doubleWord[DoubleWordSize](ESI), Pointer.Destination.doubleWord[DoubleWordSize](EDI)).encodeByte should be(Hex.lsb("F3 A5"))
        }
        s"correctly represent rep movs DWORD PTR [edi], DWORD PTR [esi] as a string" in {
          MoveString.Repeat(Pointer.Source.doubleWord[DoubleWordSize](ESI), Pointer.Destination.doubleWord[DoubleWordSize](EDI)).toString should be("rep movs DWORD PTR [edi], DWORD PTR [esi]")
        }
      }
    }

    "in long mode" when {
      import ProcessorMode.Long._

      "not repeated" should {
        s"correctly encode movs WORD PTR [rdi], WORD PTR [rsi]" in {
          MoveString(Pointer.Source.quadWord[WordSize](RSI), Pointer.Destination.quadWord[WordSize](RDI)).encodeByte should be(Hex.lsb("66 A5"))
        }
        s"correctly represent movs WORD PTR [rdi], WORD PTR [rsi] as a string" in {
          MoveString(Pointer.Source.quadWord[WordSize](RSI), Pointer.Destination.quadWord[WordSize](RDI)).toString should be("movs WORD PTR [rdi], WORD PTR [rsi]")
        }

        s"correctly encode movs DWORD PTR [rdi], DWORD PTR [rsi]" in {
          MoveString(Pointer.Source.quadWord[DoubleWordSize](RSI), Pointer.Destination.quadWord[DoubleWordSize](RDI)).encodeByte should be(Hex.lsb("A5"))
        }
        s"correctly represent movs DWORD PTR [rdi], DWORD PTR [rsi] as a string" in {
          MoveString(Pointer.Source.quadWord[DoubleWordSize](RSI), Pointer.Destination.quadWord[DoubleWordSize](RDI)).toString should be("movs DWORD PTR [rdi], DWORD PTR [rsi]")
        }

        s"correctly encode movs QWORD PTR [rdi], QWORD PTR [rsi]" in {
          MoveString(Pointer.Source.quadWord[QuadWordSize](RSI), Pointer.Destination.quadWord[QuadWordSize](RDI)).encodeByte should be(Hex.lsb("48 A5"))
        }
        s"correctly represent movs QWORD PTR [rdi], QWORD PTR [rsi] as a string" in {
          MoveString(Pointer.Source.quadWord[QuadWordSize](RSI), Pointer.Destination.quadWord[QuadWordSize](RDI)).toString should be("movs QWORD PTR [rdi], QWORD PTR [rsi]")
        }

        s"correctly encode movs QWORD PTR [edi], QWORD PTR [esi]" in {
          MoveString(Pointer.Source.doubleWord[QuadWordSize](ESI), Pointer.Destination.doubleWord[QuadWordSize](EDI)).encodeByte should be(Hex.lsb("67 48 A5"))
        }
        s"correctly represent movs QWORD PTR [edi], QWORD PTR [esi] as a string" in {
          MoveString(Pointer.Source.doubleWord[QuadWordSize](ESI), Pointer.Destination.doubleWord[QuadWordSize](EDI)).toString should be("movs QWORD PTR [edi], QWORD PTR [esi]")
        }
      }
    }
  }

  "an OutString outstruction" when {
    "in real mode" when {
      import ProcessorMode.Real._

      "not repeated" should {


        s"correctly encode outs dx, BYTE PTR [si]" in {
          OutString(Pointer.Source.word[ByteSize](SI), DX).encodeByte should be(Hex.lsb("6E"))
        }
        s"correctly represent outs dx, BYTE PTR [si] as a string" in {
          OutString(Pointer.Source.word[ByteSize](SI), DX).toString should be("outs dx, BYTE PTR [si]")
        }

        s"correctly encode outs dx, BYTE PTR [esi]" in {
          OutString(Pointer.Source.doubleWord[ByteSize](ESI), DX).encodeByte should be(Hex.lsb("67 6E"))
        }
        s"correctly represent outs dx, BYTE PTR [esi] as a string" in {
          OutString(Pointer.Source.doubleWord[ByteSize](ESI), DX).toString should be("outs dx, BYTE PTR [esi]")
        }

        s"correctly encode outs dx, WORD PTR [si]" in {
          OutString(Pointer.Source.word[WordSize](SI), DX).encodeByte should be(Hex.lsb("6F"))
        }
        s"correctly represent outs dx, WORD PTR [si] as a string" in {
          OutString(Pointer.Source.word[WordSize](SI), DX).toString should be("outs dx, WORD PTR [si]")
        }

        s"correctly encode outs dx, DWORD PTR [si]" in {
          OutString(Pointer.Source.word[DoubleWordSize](SI), DX).encodeByte should be(Hex.lsb("66 6F"))
        }
        s"correctly represent outs dx, DWORD PTR [si] as a string" in {
          OutString(Pointer.Source.word[DoubleWordSize](SI), DX).toString should be("outs dx, DWORD PTR [si]")
        }
      }

      "when repeated" should {
        s"correctly encode rep outs dx, BYTE PTR [si]" in {
          OutString.Repeat(Pointer.Source.word[ByteSize](SI), DX).encodeByte should be(Hex.lsb("F3 6E"))
        }
        s"correctly represent rep outs dx, BYTE PTR [si] as a string" in {
          OutString.Repeat(Pointer.Source.word[ByteSize](SI), DX).toString should be("rep outs dx, BYTE PTR [si]")
        }

        s"correctly encode rep outs dx, WORD PTR [si]" in {
          OutString.Repeat(Pointer.Source.word[WordSize](SI), DX).encodeByte should be(Hex.lsb("F3 6F"))
        }
        s"correctly represent rep outs dx, WORD PTR [si] as a string" in {
          OutString.Repeat(Pointer.Source.word[WordSize](SI), DX).toString should be("rep outs dx, WORD PTR [si]")
        }

        s"correctly encode rep outs dx, DWORD PTR [si]" in {
          OutString.Repeat(Pointer.Source.word[DoubleWordSize](SI), DX).encodeByte should be(Hex.lsb("F3 66 6F"))
        }
        s"correctly represent rep outs dx, DWORD PTR [si] as a string" in {
          OutString.Repeat(Pointer.Source.word[DoubleWordSize](SI), DX).toString should be("rep outs dx, DWORD PTR [si]")
        }
      }
    }

    "in protected mode" when {
      import ProcessorMode.Protected._

      "not repeated" should {

        s"correctly encode outs dx, WORD PTR [esi]" in {
          OutString(Pointer.Source.doubleWord[WordSize](ESI), DX).encodeByte should be(Hex.lsb("66 6F"))
        }
        s"correctly represent outs dx, WORD PTR [esi] as a string" in {
          OutString(Pointer.Source.doubleWord[WordSize](ESI), DX).toString should be("outs dx, WORD PTR [esi]")
        }

        s"correctly encode outs dx, DWORD PTR [esi]" in {
          OutString(Pointer.Source.doubleWord[DoubleWordSize](ESI), DX).encodeByte should be(Hex.lsb("6F"))
        }
        s"correctly represent outs dx, DWORD PTR [esi] as a string" in {
          OutString(Pointer.Source.doubleWord[DoubleWordSize](ESI), DX).toString should be("outs dx, DWORD PTR [esi]")
        }
      }

      "when repeated" should {
        s"correctly encode rep outs dx, BYTE PTR [esi]" in {
          OutString.Repeat(Pointer.Source.doubleWord[ByteSize](ESI), DX).encodeByte should be(Hex.lsb("F3 6E"))
        }
        s"correctly represent rep outs dx, BYTE PTR [esi] as a string" in {
          OutString.Repeat(Pointer.Source.doubleWord[ByteSize](ESI), DX).toString should be("rep outs dx, BYTE PTR [esi]")
        }

        s"correctly encode rep outs dx, WORD PTR [esi]" in {
          OutString.Repeat(Pointer.Source.doubleWord[WordSize](ESI), DX).encodeByte should be(Hex.lsb("F3 66 6F"))
        }
        s"correctly represent rep outs dx, WORD PTR [esi] as a string" in {
          OutString.Repeat(Pointer.Source.doubleWord[WordSize](ESI), DX).toString should be("rep outs dx, WORD PTR [esi]")
        }

        s"correctly encode rep outs dx, DWORD PTR [esi]" in {
          OutString.Repeat(Pointer.Source.doubleWord[DoubleWordSize](ESI), DX).encodeByte should be(Hex.lsb("F3 6F"))
        }
        s"correctly represent rep outs dx, DWORD PTR [esi] as a string" in {
          OutString.Repeat(Pointer.Source.doubleWord[DoubleWordSize](ESI), DX).toString should be("rep outs dx, DWORD PTR [esi]")
        }
      }
    }

    "in long mode" when {
      import ProcessorMode.Long._

      "not repeated" should {

        s"correctly encode outs dx, WORD PTR [rsi]" in {
          OutString(Pointer.Source.quadWord[WordSize](RSI), DX).encodeByte should be(Hex.lsb("66 6F"))
        }
        s"correctly represent outs dx, WORD PTR [rsi] as a string" in {
          OutString(Pointer.Source.quadWord[WordSize](RSI), DX).toString should be("outs dx, WORD PTR [rsi]")
        }

        s"correctly encode outs dx, DWORD PTR [rsi]" in {
          OutString(Pointer.Source.quadWord[DoubleWordSize](RSI), DX).encodeByte should be(Hex.lsb("6F"))
        }
        s"correctly represent outs dx, DWORD PTR [rsi] as a string" in {
          OutString(Pointer.Source.quadWord[DoubleWordSize](RSI), DX).toString should be("outs dx, DWORD PTR [rsi]")
        }

        s"correctly encode outs dx, QWORD PTR [rsi]" in {
          OutString(Pointer.Source.quadWord[QuadWordSize](RSI), DX).encodeByte should be(Hex.lsb("48 6F"))
        }
        s"correctly represent outs dx, QWORD PTR [rsi] as a string" in {
          OutString(Pointer.Source.quadWord[QuadWordSize](RSI), DX).toString should be("outs dx, QWORD PTR [rsi]")
        }

        s"correctly encode outs dx, QWORD PTR [esi]" in {
          OutString(Pointer.Source.doubleWord[QuadWordSize](ESI), DX).encodeByte should be(Hex.lsb("67 48 6F"))
        }
        s"correctly represent outs dx, QWORD PTR [esi] as a string" in {
          OutString(Pointer.Source.doubleWord[QuadWordSize](ESI), DX).toString should be("outs dx, QWORD PTR [esi]")
        }
      }
    }
  }


  "an LoadString instruction" when {
    "in real mode" when {
      import ProcessorMode.Real._

      "not repeated" should {


        s"correctly encode lods al, BYTE PTR [si]" in {
          LoadString(Pointer.Source.word[ByteSize](SI), AL).encodeByte should be(Hex.lsb("AC"))
        }
        s"correctly represent lods al, BYTE PTR [si] as a string" in {
          LoadString(Pointer.Source.word[ByteSize](SI), AL).toString should be("lods al, BYTE PTR [si]")
        }

        s"correctly encode lods al, BYTE PTR [esi]" in {
          LoadString(Pointer.Source.doubleWord[ByteSize](ESI), AL).encodeByte should be(Hex.lsb("67 AC"))
        }
        s"correctly represent lods al, BYTE PTR [esi] as a string" in {
          LoadString(Pointer.Source.doubleWord[ByteSize](ESI), AL).toString should be("lods al, BYTE PTR [esi]")
        }

        s"correctly encode lods ax, WORD PTR [si]" in {
          LoadString(Pointer.Source.word[WordSize](SI), AX).encodeByte should be(Hex.lsb("AD"))
        }
        s"correctly represent lods ax, WORD PTR [si] as a string" in {
          LoadString(Pointer.Source.word[WordSize](SI), AX).toString should be("lods ax, WORD PTR [si]")
        }

        s"correctly encode lods eax, DWORD PTR [si]" in {
          LoadString(Pointer.Source.word[DoubleWordSize](SI), EAX).encodeByte should be(Hex.lsb("66 AD"))
        }
        s"correctly represent lods eax, DWORD PTR [si] as a string" in {
          LoadString(Pointer.Source.word[DoubleWordSize](SI), EAX).toString should be("lods eax, DWORD PTR [si]")
        }
      }

      "when repeated" should {
        s"correctly encode rep lods al, BYTE PTR [si]" in {
          LoadString.Repeat(Pointer.Source.word[ByteSize](SI), AL).encodeByte should be(Hex.lsb("F3 AC"))
        }
        s"correctly represent rep lods al, BYTE PTR [si] as a string" in {
          LoadString.Repeat(Pointer.Source.word[ByteSize](SI), AL).toString should be("rep lods al, BYTE PTR [si]")
        }

        s"correctly encode rep lods ax, WORD PTR [si]" in {
          LoadString.Repeat(Pointer.Source.word[WordSize](SI), AX).encodeByte should be(Hex.lsb("F3 AD"))
        }
        s"correctly represent rep lods ax, WORD PTR [si] as a string" in {
          LoadString.Repeat(Pointer.Source.word[WordSize](SI), AX).toString should be("rep lods ax, WORD PTR [si]")
        }

        s"correctly encode rep lods eax, DWORD PTR [si]" in {
          LoadString.Repeat(Pointer.Source.word[DoubleWordSize](SI), EAX).encodeByte should be(Hex.lsb("F3 66 AD"))
        }
        s"correctly represent rep lods eax, DWORD PTR [si] as a string" in {
          LoadString.Repeat(Pointer.Source.word[DoubleWordSize](SI), EAX).toString should be("rep lods eax, DWORD PTR [si]")
        }
      }
    }

    "in protected mode" when {
      import ProcessorMode.Protected._

      "not repeated" should {

        s"correctly encode lods ax, WORD PTR [esi]" in {
          LoadString(Pointer.Source.doubleWord[WordSize](ESI), AX).encodeByte should be(Hex.lsb("66 AD"))
        }
        s"correctly represent lods ax, WORD PTR [esi] as a string" in {
          LoadString(Pointer.Source.doubleWord[WordSize](ESI), AX).toString should be("lods ax, WORD PTR [esi]")
        }

        s"correctly encode lods eax, DWORD PTR [esi]" in {
          LoadString(Pointer.Source.doubleWord[DoubleWordSize](ESI), EAX).encodeByte should be(Hex.lsb("AD"))
        }
        s"correctly represent lods eax, DWORD PTR [esi] as a string" in {
          LoadString(Pointer.Source.doubleWord[DoubleWordSize](ESI), EAX).toString should be("lods eax, DWORD PTR [esi]")
        }
      }

      "when repeated" should {
        s"correctly encode rep lods al, BYTE PTR [esi]" in {
          LoadString.Repeat(Pointer.Source.doubleWord[ByteSize](ESI), AL).encodeByte should be(Hex.lsb("F3 AC"))
        }
        s"correctly represent rep lods al, BYTE PTR [esi] as a string" in {
          LoadString.Repeat(Pointer.Source.doubleWord[ByteSize](ESI), AL).toString should be("rep lods al, BYTE PTR [esi]")
        }

        s"correctly encode rep lods ax, WORD PTR [esi]" in {
          LoadString.Repeat(Pointer.Source.doubleWord[WordSize](ESI), AX).encodeByte should be(Hex.lsb("F3 66 AD"))
        }
        s"correctly represent rep lods ax, WORD PTR [esi] as a string" in {
          LoadString.Repeat(Pointer.Source.doubleWord[WordSize](ESI), AX).toString should be("rep lods ax, WORD PTR [esi]")
        }

        s"correctly encode rep lods eax, DWORD PTR [esi]" in {
          LoadString.Repeat(Pointer.Source.doubleWord[DoubleWordSize](ESI), EAX).encodeByte should be(Hex.lsb("F3 AD"))
        }
        s"correctly represent rep lods eax, DWORD PTR [esi] as a string" in {
          LoadString.Repeat(Pointer.Source.doubleWord[DoubleWordSize](ESI), EAX).toString should be("rep lods eax, DWORD PTR [esi]")
        }
      }
    }

    "in long mode" when {
      import ProcessorMode.Long._

      "not repeated" should {

        s"correctly encode lods ax, WORD PTR [rsi]" in {
          LoadString(Pointer.Source.quadWord[WordSize](RSI), AX).encodeByte should be(Hex.lsb("66 AD"))
        }
        s"correctly represent lods ax, WORD PTR [rsi] as a string" in {
          LoadString(Pointer.Source.quadWord[WordSize](RSI), AX).toString should be("lods ax, WORD PTR [rsi]")
        }

        s"correctly encode lods eax, DWORD PTR [rsi]" in {
          LoadString(Pointer.Source.quadWord[DoubleWordSize](RSI), EAX).encodeByte should be(Hex.lsb("AD"))
        }
        s"correctly represent lods eax, DWORD PTR [rsi] as a string" in {
          LoadString(Pointer.Source.quadWord[DoubleWordSize](RSI), EAX).toString should be("lods eax, DWORD PTR [rsi]")
        }

        s"correctly encode lods rax, QWORD PTR [rsi]" in {
          LoadString(Pointer.Source.quadWord[QuadWordSize](RSI), RAX).encodeByte should be(Hex.lsb("48 AD"))
        }
        s"correctly represent lods rax, QWORD PTR [rsi] as a string" in {
          LoadString(Pointer.Source.quadWord[QuadWordSize](RSI), RAX).toString should be("lods rax, QWORD PTR [rsi]")
        }

        s"correctly encode lods rax, QWORD PTR [esi]" in {
          LoadString(Pointer.Source.doubleWord[QuadWordSize](ESI), RAX).encodeByte should be(Hex.lsb("67 48 AD"))
        }
        s"correctly represent lods rax, QWORD PTR [esi] as a string" in {
          LoadString(Pointer.Source.doubleWord[QuadWordSize](ESI), RAX).toString should be("lods rax, QWORD PTR [esi]")
        }
      }
    }

    "an StoreString instruction" when {
      "in real mode" when {
        import ProcessorMode.Real._

        "not repeated" should {
          s"correctly encode stos BYTE PTR [di], al" in {
            StoreString(AL, Pointer.Destination.word[ByteSize](DI)).encodeByte should be(Hex.lsb("AA"))
          }
          s"correctly represent stos BYTE PTR [di], al as a string" in {
            StoreString(AL, Pointer.Destination.word[ByteSize](DI)).toString should be("stos BYTE PTR [di], al")
          }

          s"correctly encode stos BYTE PTR [edi], al" in {
            StoreString(AL, Pointer.Destination.doubleWord[ByteSize](EDI)).encodeByte should be(Hex.lsb("67 AA"))
          }
          s"correctly represent stos BYTE PTR [edi], al as a string" in {
            StoreString(AL, Pointer.Destination.doubleWord[ByteSize](EDI)).toString should be("stos BYTE PTR [edi], al")
          }

          s"correctly encode stos WORD PTR [di], ax" in {
            StoreString(AX, Pointer.Destination.word[WordSize](DI)).encodeByte should be(Hex.lsb("AB"))
          }
          s"correctly represent stos WORD PTR [di], ax as a string" in {
            StoreString(AX, Pointer.Destination.word[WordSize](DI)).toString should be("stos WORD PTR [di], ax")
          }

          s"correctly encode stos DWORD PTR [di], eax" in {
            StoreString(EAX, Pointer.Destination.word[DoubleWordSize](DI)).encodeByte should be(Hex.lsb("66 AB"))
          }
          s"correctly represent stos DWORD PTR [di], eax as a string" in {
            StoreString(EAX, Pointer.Destination.word[DoubleWordSize](DI)).toString should be("stos DWORD PTR [di], eax")
          }
        }

        "when repeated" should {
          s"correctly encode rep stos BYTE PTR [di], al" in {
            StoreString.Repeat(AL, Pointer.Destination.word[ByteSize](DI)).encodeByte should be(Hex.lsb("F3 AA"))
          }
          s"correctly represent rep stos BYTE PTR [di], al as a string" in {
            StoreString.Repeat(AL, Pointer.Destination.word[ByteSize](DI)).toString should be("rep stos BYTE PTR [di], al")
          }

          s"correctly encode rep stos WORD PTR [di], ax" in {
            StoreString.Repeat(AX, Pointer.Destination.word[WordSize](DI)).encodeByte should be(Hex.lsb("F3 AB"))
          }
          s"correctly represent rep stos WORD PTR [di], ax as a string" in {
            StoreString.Repeat(AX, Pointer.Destination.word[WordSize](DI)).toString should be("rep stos WORD PTR [di], ax")
          }

          s"correctly encode rep stos DWORD PTR [di], eax" in {
            StoreString.Repeat(EAX, Pointer.Destination.word[DoubleWordSize](DI)).encodeByte should be(Hex.lsb("F3 66 AB"))
          }
          s"correctly represent rep stos DWORD PTR [di], eax as a string" in {
            StoreString.Repeat(EAX, Pointer.Destination.word[DoubleWordSize](DI)).toString should be("rep stos DWORD PTR [di], eax")
          }
        }
      }

      "in protected mode" when {
        import ProcessorMode.Protected._

        "not repeated" should {

          s"correctly encode stos WORD PTR [edi], ax" in {
            StoreString(AX, Pointer.Destination.doubleWord[WordSize](EDI)).encodeByte should be(Hex.lsb("66 AB"))
          }
          s"correctly represent stos WORD PTR [edi], ax as a string" in {
            StoreString(AX, Pointer.Destination.doubleWord[WordSize](EDI)).toString should be("stos WORD PTR [edi], ax")
          }

          s"correctly encode stos DWORD PTR [edi], eax" in {
            StoreString(EAX, Pointer.Destination.doubleWord[DoubleWordSize](EDI)).encodeByte should be(Hex.lsb("AB"))
          }
          s"correctly represent stos DWORD PTR [edi], eax as a string" in {
            StoreString(EAX, Pointer.Destination.doubleWord[DoubleWordSize](EDI)).toString should be("stos DWORD PTR [edi], eax")
          }
        }

        "when repeated" should {
          s"correctly encode rep stos BYTE PTR [edi], al" in {
            StoreString.Repeat(AL, Pointer.Destination.doubleWord[ByteSize](EDI)).encodeByte should be(Hex.lsb("F3 AA"))
          }
          s"correctly represent rep stos BYTE PTR [edi], al as a string" in {
            StoreString.Repeat(AL, Pointer.Destination.doubleWord[ByteSize](EDI)).toString should be("rep stos BYTE PTR [edi], al")
          }

          s"correctly encode rep stos WORD PTR [edi], ax" in {
            StoreString.Repeat(AX, Pointer.Destination.doubleWord[WordSize](EDI)).encodeByte should be(Hex.lsb("F3 66 AB"))
          }
          s"correctly represent rep stos WORD PTR [edi], ax as a string" in {
            StoreString.Repeat(AX, Pointer.Destination.doubleWord[WordSize](EDI)).toString should be("rep stos WORD PTR [edi], ax")
          }

          s"correctly encode rep stos DWORD PTR [edi], eax" in {
            StoreString.Repeat(EAX, Pointer.Destination.doubleWord[DoubleWordSize](EDI)).encodeByte should be(Hex.lsb("F3 AB"))
          }
          s"correctly represent rep stos DWORD PTR [edi], eax as a string" in {
            StoreString.Repeat(EAX, Pointer.Destination.doubleWord[DoubleWordSize](EDI)).toString should be("rep stos DWORD PTR [edi], eax")
          }
        }
      }

      "in long mode" when {
        import ProcessorMode.Long._

        "not repeated" should {

          s"correctly encode stos WORD PTR [rdi], ax" in {
            StoreString(AX, Pointer.Destination.quadWord[WordSize](RDI)).encodeByte should be(Hex.lsb("66 AB"))
          }
          s"correctly represent stos WORD PTR [rdi], ax as a string" in {
            StoreString(AX, Pointer.Destination.quadWord[WordSize](RDI)).toString should be("stos WORD PTR [rdi], ax")
          }

          s"correctly encode stos DWORD PTR [rdi], eax" in {
            StoreString(EAX, Pointer.Destination.quadWord[DoubleWordSize](RDI)).encodeByte should be(Hex.lsb("AB"))
          }
          s"correctly represent stos DWORD PTR [rdi], eax as a string" in {
            StoreString(EAX, Pointer.Destination.quadWord[DoubleWordSize](RDI)).toString should be("stos DWORD PTR [rdi], eax")
          }

          s"correctly encode stos QWORD PTR [rdi], rax" in {
            StoreString(RAX, Pointer.Destination.quadWord[QuadWordSize](RDI)).encodeByte should be(Hex.lsb("48 AB"))
          }
          s"correctly represent stos QWORD PTR [rdi], rax as a string" in {
            StoreString(RAX, Pointer.Destination.quadWord[QuadWordSize](RDI)).toString should be("stos QWORD PTR [rdi], rax")
          }

          s"correctly encode stos QWORD PTR [edi], rax" in {
            StoreString(RAX, Pointer.Destination.doubleWord[QuadWordSize](EDI)).encodeByte should be(Hex.lsb("67 48 AB"))
          }
          s"correctly represent stos QWORD PTR [edi], rax as a string" in {
            StoreString(RAX, Pointer.Destination.doubleWord[QuadWordSize](EDI)).toString should be("stos QWORD PTR [edi], rax")
          }
        }
      }
    }
  }


  "an CompareString instruction" when {
    "in legacy mode" should {

      import ProcessorMode.Legacy._

      "correctly encode stos BYTE PTR [edi], al" in {
        StoreString(AL, Pointer.Destination.word[ByteSize](DI)).encodeByte should be(Hex.lsb("AA"))
      }

      "correctly encode REP stos BYTE PTR [edi], al" in {
        StoreString.Repeat(AL, Pointer.Destination.word[ByteSize](DI)).encodeByte should be(Hex.lsb("F3 AA"))
      }
    }


    "in real mode" when {
      import ProcessorMode.Real._

      "not repeated" should {


        s"correctly encode cmps BYTE PTR [di], BYTE PTR [si]" in {
          CompareString(Pointer.Source.word[ByteSize](SI), Pointer.Destination.word[ByteSize](DI)).encodeByte should be(Hex.lsb("A6"))
        }
        s"correctly represent cmps BYTE PTR [di], BYTE PTR [si] as a string" in {
          CompareString(Pointer.Source.word[ByteSize](SI), Pointer.Destination.word[ByteSize](DI)).toString should be("cmps BYTE PTR [di], BYTE PTR [si]")
        }

        s"correctly encode cmps BYTE PTR [edi], BYTE PTR [esi]" in {
          CompareString(Pointer.Source.doubleWord[ByteSize](ESI), Pointer.Destination.doubleWord[ByteSize](EDI)).encodeByte should be(Hex.lsb("67 A6"))
        }
        s"correctly represent cmps BYTE PTR [edi], BYTE PTR [esi] as a string" in {
          CompareString(Pointer.Source.doubleWord[ByteSize](ESI), Pointer.Destination.doubleWord[ByteSize](EDI)).toString should be("cmps BYTE PTR [edi], BYTE PTR [esi]")
        }

        s"correctly encode cmps WORD PTR [di], WORD PTR [si]" in {
          CompareString(Pointer.Source.word[WordSize](SI), Pointer.Destination.word[WordSize](DI)).encodeByte should be(Hex.lsb("A7"))
        }
        s"correctly represent cmps WORD PTR [di], WORD PTR [si] as a string" in {
          CompareString(Pointer.Source.word[WordSize](SI), Pointer.Destination.word[WordSize](DI)).toString should be("cmps WORD PTR [di], WORD PTR [si]")
        }

        s"correctly encode cmps DWORD PTR [di], DWORD PTR [si]" in {
          CompareString(Pointer.Source.word[DoubleWordSize](SI), Pointer.Destination.word[DoubleWordSize](DI)).encodeByte should be(Hex.lsb("66 A7"))
        }
        s"correctly represent cmps DWORD PTR [di], DWORD PTR [si] as a string" in {
          CompareString(Pointer.Source.word[DoubleWordSize](SI), Pointer.Destination.word[DoubleWordSize](DI)).toString should be("cmps DWORD PTR [di], DWORD PTR [si]")
        }
      }

      "when repeeated" should {
        s"correctly encode repe cmps BYTE PTR [di], BYTE PTR [si]" in {
          CompareString.RepeatEqual(Pointer.Source.word[ByteSize](SI), Pointer.Destination.word[ByteSize](DI)).encodeByte should be(Hex.lsb("F3 A6"))
        }
        s"correctly represent repe cmps BYTE PTR [di], BYTE PTR [si] as a string" in {
          CompareString.RepeatEqual(Pointer.Source.word[ByteSize](SI), Pointer.Destination.word[ByteSize](DI)).toString should be("repe cmps BYTE PTR [di], BYTE PTR [si]")
        }

        s"correctly encode repe cmps WORD PTR [di], WORD PTR [si]" in {
          CompareString.RepeatEqual(Pointer.Source.word[WordSize](SI), Pointer.Destination.word[WordSize](DI)).encodeByte should be(Hex.lsb("F3 A7"))
        }
        s"correctly represent repe cmps WORD PTR [di], WORD PTR [si] as a string" in {
          CompareString.RepeatEqual(Pointer.Source.word[WordSize](SI), Pointer.Destination.word[WordSize](DI)).toString should be("repe cmps WORD PTR [di], WORD PTR [si]")
        }

        s"correctly encode repee cmps DWORD PTR [di], DWORD PTR [si]" in {
          CompareString.RepeatEqual(Pointer.Source.word[DoubleWordSize](SI), Pointer.Destination.word[DoubleWordSize](DI)).encodeByte should be(Hex.lsb("F3 66 A7"))
        }
        s"correctly represent repee cmps DWORD PTR [di], DWORD PTR [si] as a string" in {
          CompareString.RepeatEqual(Pointer.Source.word[DoubleWordSize](SI), Pointer.Destination.word[DoubleWordSize](DI)).toString should be("repe cmps DWORD PTR [di], DWORD PTR [si]")
        }

        s"correctly encode repne cmps BYTE PTR [di], BYTE PTR [si]" in {
          CompareString.RepeatNotEqual(Pointer.Source.word[ByteSize](SI), Pointer.Destination.word[ByteSize](DI)).encodeByte should be(Hex.lsb("F2 A6"))
        }
        s"correctly represent repne cmps BYTE PTR [di], BYTE PTR [si] as a string" in {
          CompareString.RepeatNotEqual(Pointer.Source.word[ByteSize](SI), Pointer.Destination.word[ByteSize](DI)).toString should be("repne cmps BYTE PTR [di], BYTE PTR [si]")
        }

        s"correctly encode repne cmps WORD PTR [di], WORD PTR [si]" in {
          CompareString.RepeatNotEqual(Pointer.Source.word[WordSize](SI), Pointer.Destination.word[WordSize](DI)).encodeByte should be(Hex.lsb("F2 A7"))
        }
        s"correctly represent repne cmps WORD PTR [di], WORD PTR [si] as a string" in {
          CompareString.RepeatNotEqual(Pointer.Source.word[WordSize](SI), Pointer.Destination.word[WordSize](DI)).toString should be("repne cmps WORD PTR [di], WORD PTR [si]")
        }

        s"correctly encode repne cmps DWORD PTR [di], DWORD PTR [si]" in {
          CompareString.RepeatNotEqual(Pointer.Source.word[DoubleWordSize](SI), Pointer.Destination.word[DoubleWordSize](DI)).encodeByte should be(Hex.lsb("F2 66 A7"))
        }
        s"correctly represent repne cmps DWORD PTR [di], DWORD PTR [si] as a string" in {
          CompareString.RepeatNotEqual(Pointer.Source.word[DoubleWordSize](SI), Pointer.Destination.word[DoubleWordSize](DI)).toString should be("repne cmps DWORD PTR [di], DWORD PTR [si]")
        }
      }
    }

    "in protected mode" when {
      import ProcessorMode.Protected._

      "not repeated" should {
        s"correctly encode cmps WORD PTR [edi], WORD PTR [esi]" in {
          CompareString(Pointer.Source.doubleWord[WordSize](ESI), Pointer.Destination.doubleWord[WordSize](EDI)).encodeByte should be(Hex.lsb("66 A7"))
        }
        s"correctly represent cmps WORD PTR [edi], WORD PTR [esi] as a string" in {
          CompareString(Pointer.Source.doubleWord[WordSize](ESI), Pointer.Destination.doubleWord[WordSize](EDI)).toString should be("cmps WORD PTR [edi], WORD PTR [esi]")
        }

        s"correctly encode cmps DWORD PTR [edi], DWORD PTR [esi]" in {
          CompareString(Pointer.Source.doubleWord[DoubleWordSize](ESI), Pointer.Destination.doubleWord[DoubleWordSize](EDI)).encodeByte should be(Hex.lsb("A7"))
        }
        s"correctly represent cmps DWORD PTR [edi], DWORD PTR [esi] as a string" in {
          CompareString(Pointer.Source.doubleWord[DoubleWordSize](ESI), Pointer.Destination.doubleWord[DoubleWordSize](EDI)).toString should be("cmps DWORD PTR [edi], DWORD PTR [esi]")
        }
      }

      "when repeated" should {
        s"correctly encode repe cmps WORD PTR [edi], WORD PTR [esi]" in {
          CompareString.RepeatEqual(Pointer.Source.doubleWord[WordSize](ESI), Pointer.Destination.doubleWord[WordSize](EDI)).encodeByte should be(Hex.lsb("F3 66 A7"))
        }
        s"correctly represent repe cmps WORD PTR [edi], WORD PTR [esi] as a string" in {
          CompareString.RepeatEqual(Pointer.Source.doubleWord[WordSize](ESI), Pointer.Destination.doubleWord[WordSize](EDI)).toString should be("repe cmps WORD PTR [edi], WORD PTR [esi]")
        }

        s"correctly encode repe cmps DWORD PTR [edi], DWORD PTR [esi]" in {
          CompareString.RepeatEqual(Pointer.Source.doubleWord[DoubleWordSize](ESI), Pointer.Destination.doubleWord[DoubleWordSize](EDI)).encodeByte should be(Hex.lsb("F3 A7"))
        }
        s"correctly represent repe cmps DWORD PTR [edi], DWORD PTR [esi] as a string" in {
          CompareString.RepeatEqual(Pointer.Source.doubleWord[DoubleWordSize](ESI), Pointer.Destination.doubleWord[DoubleWordSize](EDI)).toString should be("repe cmps DWORD PTR [edi], DWORD PTR [esi]")
        }

        s"correctly encode repne cmps WORD PTR [edi], WORD PTR [esi]" in {
          CompareString.RepeatNotEqual(Pointer.Source.doubleWord[WordSize](ESI), Pointer.Destination.doubleWord[WordSize](EDI)).encodeByte should be(Hex.lsb("F2 66 A7"))
        }
        s"correctly represent repne cmps WORD PTR [edi], WORD PTR [esi] as a string" in {
          CompareString.RepeatNotEqual(Pointer.Source.doubleWord[WordSize](ESI), Pointer.Destination.doubleWord[WordSize](EDI)).toString should be("repne cmps WORD PTR [edi], WORD PTR [esi]")
        }

        s"correctly encode repne cmps DWORD PTR [edi], DWORD PTR [esi]" in {
          CompareString.RepeatNotEqual(Pointer.Source.doubleWord[DoubleWordSize](ESI), Pointer.Destination.doubleWord[DoubleWordSize](EDI)).encodeByte should be(Hex.lsb("F2 A7"))
        }
        s"correctly represent repne cmps DWORD PTR [edi], DWORD PTR [esi] as a string" in {
          CompareString.RepeatNotEqual(Pointer.Source.doubleWord[DoubleWordSize](ESI), Pointer.Destination.doubleWord[DoubleWordSize](EDI)).toString should be("repne cmps DWORD PTR [edi], DWORD PTR [esi]")
        }
      }
    }

    "in long mode" when {
      import ProcessorMode.Long._

      "not repeated" should {
        s"correctly encode cmps WORD PTR [rdi], WORD PTR [rsi]" in {
          CompareString(Pointer.Source.quadWord[WordSize](RSI), Pointer.Destination.quadWord[WordSize](RDI)).encodeByte should be(Hex.lsb("66 A7"))
        }
        s"correctly represent cmps WORD PTR [rdi], WORD PTR [rsi] as a string" in {
          CompareString(Pointer.Source.quadWord[WordSize](RSI), Pointer.Destination.quadWord[WordSize](RDI)).toString should be("cmps WORD PTR [rdi], WORD PTR [rsi]")
        }

        s"correctly encode cmps DWORD PTR [rdi], DWORD PTR [rsi]" in {
          CompareString(Pointer.Source.quadWord[DoubleWordSize](RSI), Pointer.Destination.quadWord[DoubleWordSize](RDI)).encodeByte should be(Hex.lsb("A7"))
        }
        s"correctly represent cmps DWORD PTR [rdi], DWORD PTR [rsi] as a string" in {
          CompareString(Pointer.Source.quadWord[DoubleWordSize](RSI), Pointer.Destination.quadWord[DoubleWordSize](RDI)).toString should be("cmps DWORD PTR [rdi], DWORD PTR [rsi]")
        }

        s"correctly encode cmps QWORD PTR [rdi], QWORD PTR [rsi]" in {
          CompareString(Pointer.Source.quadWord[QuadWordSize](RSI), Pointer.Destination.quadWord[QuadWordSize](RDI)).encodeByte should be(Hex.lsb("48 A7"))
        }
        s"correctly represent cmps QWORD PTR [rdi], QWORD PTR [rsi] as a string" in {
          CompareString(Pointer.Source.quadWord[QuadWordSize](RSI), Pointer.Destination.quadWord[QuadWordSize](RDI)).toString should be("cmps QWORD PTR [rdi], QWORD PTR [rsi]")
        }

        s"correctly encode cmps QWORD PTR [edi], QWORD PTR [esi]" in {
          CompareString(Pointer.Source.doubleWord[QuadWordSize](ESI), Pointer.Destination.doubleWord[QuadWordSize](EDI)).encodeByte should be(Hex.lsb("67 48 A7"))
        }
        s"correctly represent cmps QWORD PTR [edi], QWORD PTR [esi] as a string" in {
          CompareString(Pointer.Source.doubleWord[QuadWordSize](ESI), Pointer.Destination.doubleWord[QuadWordSize](EDI)).toString should be("cmps QWORD PTR [edi], QWORD PTR [esi]")
        }
      }
    }
  }



  "an ScanString instruction" when {
    "in real mode" when {
      import ProcessorMode.Real._

      "not repeated" should {
        s"correctly encode scas BYTE PTR [di], al" in {
          ScanString(AL, Pointer.Destination.word[ByteSize](DI)).encodeByte should be(Hex.lsb("AE"))
        }
        s"correctly represent scas BYTE PTR [di], al as a string" in {
          ScanString(AL, Pointer.Destination.word[ByteSize](DI)).toString should be("scas BYTE PTR [di], al")
        }

        s"correctly encode scas BYTE PTR [edi], al" in {
          ScanString(AL, Pointer.Destination.doubleWord[ByteSize](EDI)).encodeByte should be(Hex.lsb("67 AE"))
        }
        s"correctly represent scas BYTE PTR [edi], al as a string" in {
          ScanString(AL, Pointer.Destination.doubleWord[ByteSize](EDI)).toString should be("scas BYTE PTR [edi], al")
        }

        s"correctly encode scas WORD PTR [di], ax" in {
          ScanString(AX, Pointer.Destination.word[WordSize](DI)).encodeByte should be(Hex.lsb("AF"))
        }
        s"correctly represent scas WORD PTR [di], ax as a string" in {
          ScanString(AX, Pointer.Destination.word[WordSize](DI)).toString should be("scas WORD PTR [di], ax")
        }

        s"correctly encode scas DWORD PTR [di], eax" in {
          ScanString(EAX, Pointer.Destination.word[DoubleWordSize](DI)).encodeByte should be(Hex.lsb("66 AF"))
        }
        s"correctly represent scas DWORD PTR [di], eax as a string" in {
          ScanString(EAX, Pointer.Destination.word[DoubleWordSize](DI)).toString should be("scas DWORD PTR [di], eax")
        }
      }

      "when repeated" should {
        s"correctly encode repe scas BYTE PTR [di], al" in {
          ScanString.RepeatEqual(AL, Pointer.Destination.word[ByteSize](DI)).encodeByte should be(Hex.lsb("F3 AE"))
        }
        s"correctly represent repe scas BYTE PTR [di], al as a string" in {
          ScanString.RepeatEqual(AL, Pointer.Destination.word[ByteSize](DI)).toString should be("repe scas BYTE PTR [di], al")
        }

        s"correctly encode repe scas WORD PTR [di], ax" in {
          ScanString.RepeatEqual(AX, Pointer.Destination.word[WordSize](DI)).encodeByte should be(Hex.lsb("F3 AF"))
        }
        s"correctly represent repe scas WORD PTR [di], ax as a string" in {
          ScanString.RepeatEqual(AX, Pointer.Destination.word[WordSize](DI)).toString should be("repe scas WORD PTR [di], ax")
        }

        s"correctly encode repe scas DWORD PTR [di], eax" in {
          ScanString.RepeatEqual(EAX, Pointer.Destination.word[DoubleWordSize](DI)).encodeByte should be(Hex.lsb("F3 66 AF"))
        }
        s"correctly represent repe scas DWORD PTR [di], eax as a string" in {
          ScanString.RepeatEqual(EAX, Pointer.Destination.word[DoubleWordSize](DI)).toString should be("repe scas DWORD PTR [di], eax")
        }


        s"correctly encode repne scas BYTE PTR [di], al" in {
          ScanString.RepeatNotEqual(AL, Pointer.Destination.word[ByteSize](DI)).encodeByte should be(Hex.lsb("F2 AE"))
        }
        s"correctly represent repne scas BYTE PTR [di], al as a string" in {
          ScanString.RepeatNotEqual(AL, Pointer.Destination.word[ByteSize](DI)).toString should be("repne scas BYTE PTR [di], al")
        }

        s"correctly encode repne scas WORD PTR [di], ax" in {
          ScanString.RepeatNotEqual(AX, Pointer.Destination.word[WordSize](DI)).encodeByte should be(Hex.lsb("F2 AF"))
        }
        s"correctly represent repne scas WORD PTR [di], ax as a string" in {
          ScanString.RepeatNotEqual(AX, Pointer.Destination.word[WordSize](DI)).toString should be("repne scas WORD PTR [di], ax")
        }

        s"correctly encode repne scas DWORD PTR [di], eax" in {
          ScanString.RepeatNotEqual(EAX, Pointer.Destination.word[DoubleWordSize](DI)).encodeByte should be(Hex.lsb("F2 66 AF"))
        }
        s"correctly represent repne scas DWORD PTR [di], eax as a string" in {
          ScanString.RepeatNotEqual(EAX, Pointer.Destination.word[DoubleWordSize](DI)).toString should be("repne scas DWORD PTR [di], eax")
        }
      }
    }

    "in protected mode" when {
      import ProcessorMode.Protected._

      "not repeated" should {

        s"correctly encode scas WORD PTR [edi], ax" in {
          ScanString(AX, Pointer.Destination.doubleWord[WordSize](EDI)).encodeByte should be(Hex.lsb("66 AF"))
        }
        s"correctly represent scas WORD PTR [edi], ax as a string" in {
          ScanString(AX, Pointer.Destination.doubleWord[WordSize](EDI)).toString should be("scas WORD PTR [edi], ax")
        }

        s"correctly encode scas DWORD PTR [edi], eax" in {
          ScanString(EAX, Pointer.Destination.doubleWord[DoubleWordSize](EDI)).encodeByte should be(Hex.lsb("AF"))
        }
        s"correctly represent scas DWORD PTR [edi], eax as a string" in {
          ScanString(EAX, Pointer.Destination.doubleWord[DoubleWordSize](EDI)).toString should be("scas DWORD PTR [edi], eax")
        }
      }

      "when repeated" should {
        s"correctly encode repe scas BYTE PTR [edi], al" in {
          ScanString.RepeatEqual(AL, Pointer.Destination.doubleWord[ByteSize](EDI)).encodeByte should be(Hex.lsb("F3 AE"))
        }
        s"correctly represent repe scas BYTE PTR [edi], al as a string" in {
          ScanString.RepeatEqual(AL, Pointer.Destination.doubleWord[ByteSize](EDI)).toString should be("repe scas BYTE PTR [edi], al")
        }

        s"correctly encode repe scas WORD PTR [edi], ax" in {
          ScanString.RepeatEqual(AX, Pointer.Destination.doubleWord[WordSize](EDI)).encodeByte should be(Hex.lsb("F3 66 AF"))
        }
        s"correctly represent repe scas WORD PTR [edi], ax as a string" in {
          ScanString.RepeatEqual(AX, Pointer.Destination.doubleWord[WordSize](EDI)).toString should be("repe scas WORD PTR [edi], ax")
        }

        s"correctly encode repe scas DWORD PTR [edi], eax" in {
          ScanString.RepeatEqual(EAX, Pointer.Destination.doubleWord[DoubleWordSize](EDI)).encodeByte should be(Hex.lsb("F3 AF"))
        }
        s"correctly represent repe scas DWORD PTR [edi], eax as a string" in {
          ScanString.RepeatEqual(EAX, Pointer.Destination.doubleWord[DoubleWordSize](EDI)).toString should be("repe scas DWORD PTR [edi], eax")
        }


        s"correctly encode repne scas BYTE PTR [edi], al" in {
          ScanString.RepeatNotEqual(AL, Pointer.Destination.doubleWord[ByteSize](EDI)).encodeByte should be(Hex.lsb("F2 AE"))
        }
        s"correctly represent repne scas BYTE PTR [edi], al as a string" in {
          ScanString.RepeatNotEqual(AL, Pointer.Destination.doubleWord[ByteSize](EDI)).toString should be("repne scas BYTE PTR [edi], al")
        }

        s"correctly encode repne scas WORD PTR [edi], ax" in {
          ScanString.RepeatNotEqual(AX, Pointer.Destination.doubleWord[WordSize](EDI)).encodeByte should be(Hex.lsb("F2 66 AF"))
        }
        s"correctly represent repne scas WORD PTR [edi], ax as a string" in {
          ScanString.RepeatNotEqual(AX, Pointer.Destination.doubleWord[WordSize](EDI)).toString should be("repne scas WORD PTR [edi], ax")
        }

        s"correctly encode repne scas DWORD PTR [edi], eax" in {
          ScanString.RepeatNotEqual(EAX, Pointer.Destination.doubleWord[DoubleWordSize](EDI)).encodeByte should be(Hex.lsb("F2 AF"))
        }
        s"correctly represent repne scas DWORD PTR [edi], eax as a string" in {
          ScanString.RepeatNotEqual(EAX, Pointer.Destination.doubleWord[DoubleWordSize](EDI)).toString should be("repne scas DWORD PTR [edi], eax")
        }
      }
    }

    "in long mode" when {
      import ProcessorMode.Long._

      "not repeated" should {

        s"correctly encode scas WORD PTR [rdi], ax" in {
          ScanString(AX, Pointer.Destination.quadWord[WordSize](RDI)).encodeByte should be(Hex.lsb("66 AF"))
        }
        s"correctly represent scas WORD PTR [rdi], ax as a string" in {
          ScanString(AX, Pointer.Destination.quadWord[WordSize](RDI)).toString should be("scas WORD PTR [rdi], ax")
        }

        s"correctly encode scas DWORD PTR [rdi], eax" in {
          ScanString(EAX, Pointer.Destination.quadWord[DoubleWordSize](RDI)).encodeByte should be(Hex.lsb("AF"))
        }
        s"correctly represent scas DWORD PTR [rdi], eax as a string" in {
          ScanString(EAX, Pointer.Destination.quadWord[DoubleWordSize](RDI)).toString should be("scas DWORD PTR [rdi], eax")
        }

        s"correctly encode scas QWORD PTR [rdi], rax" in {
          ScanString(RAX, Pointer.Destination.quadWord[QuadWordSize](RDI)).encodeByte should be(Hex.lsb("48 AF"))
        }
        s"correctly represent scas QWORD PTR [rdi], rax as a string" in {
          ScanString(RAX, Pointer.Destination.quadWord[QuadWordSize](RDI)).toString should be("scas QWORD PTR [rdi], rax")
        }

        s"correctly encode scas QWORD PTR [edi], rax" in {
          ScanString(RAX, Pointer.Destination.doubleWord[QuadWordSize](EDI)).encodeByte should be(Hex.lsb("67 48 AF"))
        }
        s"correctly represent scas QWORD PTR [edi], rax as a string" in {
          ScanString(RAX, Pointer.Destination.doubleWord[QuadWordSize](EDI)).toString should be("scas QWORD PTR [edi], rax")
        }
      }
    }
  }
}