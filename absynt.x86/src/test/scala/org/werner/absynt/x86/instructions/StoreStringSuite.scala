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
import org.werner.absynt.x86.operands.memoryaccess.DestinationReference
import org.werner.absynt.x86.operands.{ByteSize, DoubleWordSize, WordSize}
import org.scalatest.{Matchers, WordSpec}

class StoreStringSuite extends WordSpec with Matchers {

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

      "correctly encode stos BYTE PTR [di], al" in {
        StoreString(AL, DestinationReference[ByteSize](DI)).encodeByte should be(Hex.lsb("AA"))
      }

      "correctly represent stos BYTE PTR [di], al as a string" in {
        StoreString(AL, DestinationReference[ByteSize](DI)).toString should be("stos BYTE PTR [di], al")
      }

      "correctly encode REP stos BYTE PTR [di], al" in {
        StoreString.Repeat(AL, DestinationReference[ByteSize](DI)).encodeByte should be(Hex.lsb("F3 AA"))
      }

      "correctly represent rep stos BYTE PTR [di], al as a string" in {
        StoreString.Repeat(AL, DestinationReference[ByteSize](DI)).toString should be("rep stos BYTE PTR [di], al")
      }

      "correctly encode REP stos WORD PTR [di], ax" in {
        StoreString.Repeat(AX, DestinationReference[WordSize](DI)).encodeByte should be(Hex.lsb("F3 AB"))
      }

      "correctly represent rep stos WORD PTR [di], ax as a string" in {
        StoreString.Repeat(AX, DestinationReference[WordSize](DI)).toString should be("rep stos WORD PTR [di], ax")
      }

      "correctly encode stos WORD PTR [di], ax" in {
        StoreString(AX, DestinationReference[WordSize](DI)).encodeByte should be(Hex.lsb("AB"))
      }

      "correctly represent stos WORD PTR [di], ax as a string" in {
        StoreString(AX, DestinationReference[WordSize](DI)).toString should be("stos WORD PTR [di], ax")
      }

      "correctly encode stos WORD PTR [edi], ax" in {
        StoreString(AX, DestinationReference[WordSize](EDI)).encodeByte should be(Hex.lsb("67 AB"))
      }

      "correctly represent rep stos WORD PTR [edi], ax as a string" in {
        StoreString.Repeat(AX, DestinationReference[WordSize](EDI)).toString should be("rep stos WORD PTR [edi], ax")
      }

      "correctly encode stos WORD PTR [di], eax" in {
        StoreString(EAX, DestinationReference[WordSize](DI)).encodeByte should be(Hex.lsb("66 AB"))
      }

      "correctly represent stos WORD PTR [di], eax as a string" in {
        StoreString(EAX, DestinationReference[WordSize](DI)).toString should be("stos WORD PTR [di], eax")
      }

      "correctly encode stos WORD PTR [edi], eax" in {
        StoreString(EAX, DestinationReference[WordSize](EDI)).encodeByte should be(Hex.lsb("67 66 AB"))
      }

      "correctly represent stos WORD PTR [edi], eax as a string" in {
        StoreString(EAX, DestinationReference[WordSize](EDI)).toString should be("stos WORD PTR [edi], eax")
      }
    }

    "in protected mode" should {

      import ProcessorMode.Protected._

      "correctly encode stos BYTE PTR [edi], al" in {
        StoreString(AL, DestinationReference[ByteSize](EDI)).encodeByte should be(Hex.lsb("AA"))
      }

      "correctly encode REP stos BYTE PTR [edi], al" in {
        StoreString.Repeat(AL, DestinationReference[ByteSize](EDI)).encodeByte should be(Hex.lsb("F3 AA"))
      }

      "correctly encode stos BYTE PTR [di], al" in {
        StoreString(AL, DestinationReference[ByteSize](DI)).encodeByte should be(Hex.lsb("67 AA"))
      }

      "correctly encode stos WORD PTR [di], ax" in {
        StoreString(AX, DestinationReference[WordSize](DI)).encodeByte should be(Hex.lsb("67 66 AB"))
      }

      "correctly encode stos WORD PTR [di], eax" in {
        StoreString(EAX, DestinationReference[DoubleWordSize](DI)).encodeByte should be(Hex.lsb("67 AB"))
      }
    }

    "in long mode" should {

      import ProcessorMode.Long._

      "correctly encode stos BYTE PTR [edi], al" in {
        StoreString(AL, DestinationReference[ByteSize](EDI)).encodeByte should be(Hex.lsb("67 AA"))
      }

      "correctly encode REP stos BYTE PTR [edi], al" in {
        StoreString.Repeat(AL, DestinationReference[ByteSize](EDI)).encodeByte should be(Hex.lsb("F3 67 AA"))
      }

      "correctly encode stos BYTE PTR [rdi], al" in {
        StoreString(AL, DestinationReference[ByteSize](RDI)).encodeByte should be(Hex.lsb("AA"))
      }

      "correctly encode stos WORD PTR [di], ax" in {
        StoreString(AX, DestinationReference[WordSize](RDI)).encodeByte should be(Hex.lsb("66 AB"))
      }

      "correctly encode stos WORD PTR [di], eax" in {
        StoreString(EAX, DestinationReference[DoubleWordSize](RDI)).encodeByte should be(Hex.lsb("AB"))
      }
    }
  }
}