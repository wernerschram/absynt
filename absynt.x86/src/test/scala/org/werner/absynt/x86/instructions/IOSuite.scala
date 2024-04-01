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
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scala.language.implicitConversions

class IOSuite extends AnyWordSpec with Matchers {

  "an Input instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode in al, 0x10" in {
        Input(0x10.toByte, AL).encodeByte should be(Hex.lsb("E4 10"))
      }

      "correctly represent in al, 0x10 as a string" in {
        Input(0x10.toByte, AL).toString should be("in al, 16")
      }

      "correctly encode in ax, 0x20" in {
        Input(0x20.toByte, AX).encodeByte should be(Hex.lsb("E5 20"))
      }

      "correctly encode in eax, 0x20" in {
        Input(0x20.toByte, EAX).encodeByte should be(Hex.lsb("66 E5 20"))
      }

      "correctly represent in ax, 0x40 as a string" in {
        Input(0x40.toByte, AX).toString should be("in ax, 64")
      }

      "correctly represent in eax, 0x40 as a string" in {
        Input(0x40.toByte, EAX).toString should be("in eax, 64")
      }

      "correctly encode in al, dx" in {
        Input(DX, AL).encodeByte should be(Hex.lsb("EC"))
      }

      "correctly represent in al, dx as a string" in {
        Input(DX, AL).toString should be("in al, dx")
      }

      "correctly encode in ax, dx" in {
        Input(DX, AX).encodeByte should be(Hex.lsb("ED"))
      }

      "correctly represent in ax, dx as a string" in {
        Input(DX, AX).toString should be("in ax, dx")
      }

      "correctly encode in eax, dx" in {
        Input(DX, EAX).encodeByte should be(Hex.lsb("66 ED"))
      }

      "correctly represent in eax, dx as a string" in {
        Input(DX, EAX).toString should be("in eax, dx")
      }
    }
    "in protected mode" should {
      import ProcessorMode.Protected._

      "correctly encode in al, 0x10" in {
        Input(0x10.toByte, AL).encodeByte should be(Hex.lsb("E4 10"))
      }

      "correctly encode in ax, 0x40" in {
        Input(0x20.toByte, AX).encodeByte should be(Hex.lsb("66 E5 20"))
      }

      "correctly encode in eax, 0x40" in {
        Input(0x20.toByte, EAX).encodeByte should be(Hex.lsb("E5 20"))
      }

      "correctly encode in al, dx" in {
        Input(DX, AL).encodeByte should be(Hex.lsb("EC"))
      }

      "correctly encode in ax, dx" in {
        Input(DX, AX).encodeByte should be(Hex.lsb("66 ED"))
      }

      "correctly encode in eax, dx" in {
        Input(DX, EAX).encodeByte should be(Hex.lsb("ED"))
      }

    }
    "in long mode" should {

      import ProcessorMode.Long._

      "correctly encode in al, 0x10" in {
        Input(0x10.toByte, AL).encodeByte should be(Hex.lsb("E4 10"))
      }

      "correctly encode in ax, 0x40" in {
        Input(0x20.toByte, AX).encodeByte should be(Hex.lsb("66 E5 20"))
      }

      "correctly encode in eax, 0x40" in {
        Input(0x20.toByte, EAX).encodeByte should be(Hex.lsb("E5 20"))
      }

      "correctly encode in al, dx" in {
        Input(DX, AL).encodeByte should be(Hex.lsb("EC"))
      }

      "correctly encode in ax, dx" in {
        Input(DX, AX).encodeByte should be(Hex.lsb("66 ED"))
      }

      "correctly encode in eax, dx" in {
        Input(DX, EAX).encodeByte should be(Hex.lsb("ED"))
      }
    }
  }

  "an Output instruction" when {
    "in real mode" should {

      import ProcessorMode.Real._

      "correctly encode out 0x10, al" in {
        Output(AL, 0x10.toByte).encodeByte should be(Hex.lsb("E6 10"))
      }

      "correctly represent out 0x10, al as a string" in {
        Output(AL, 0x10.toByte).toString should be("out 16, al")
      }

      "correctly encode out 0x20, ax" in {
        Output(AX, 0x20.toByte).encodeByte should be(Hex.lsb("E7 20"))
      }

      "correctly encode out 0x20, eax" in {
        Output(EAX, 0x20.toByte).encodeByte should be(Hex.lsb("66 E7 20"))
      }

      "correctly represent out 0x20, ax as a string" in {
        Output(AX, 0x20.toByte).toString should be("out 32, ax")
      }

      "correctly encode out dx, al" in {
        Output(AL, DX).encodeByte should be(Hex.lsb("EE"))
      }

      "correctly represent out dx, al as a string" in {
        Output(AL, DX).toString should be("out dx, al")
      }

      "correctly encode out dx, ax" in {
        Output(AX, DX).encodeByte should be(Hex.lsb("EF"))
      }

      "correctly represent out dx, ax as a string" in {
        Output(AX, DX).toString should be("out dx, ax")
      }

      "correctly encode out dx, eax" in {
        Output(EAX, DX).encodeByte should be(Hex.lsb("66 EF"))
      }

      "correctly represent out dx, eax as a string" in {
        Output(EAX, DX).toString should be("out dx, eax")
      }
    }
    "in protected mode" should {
      import ProcessorMode.Protected._

      "correctly encode out 0x10, al" in {
        Output(AL, 0x10.toByte).encodeByte should be(Hex.lsb("E6 10"))
      }

      "correctly encode out 0x20, ax" in {
        Output(AX, 0x20.toByte).encodeByte should be(Hex.lsb("66 E7 20"))
      }

      "correctly encode out 0x40, eax" in {
        Output(EAX, 0x40.toByte).encodeByte should be(Hex.lsb("E7 40"))
      }

      "correctly encode out dx, al" in {
        Output(AL, DX).encodeByte should be(Hex.lsb("EE"))
      }

      "correctly encode out dx, ax" in {
        Output(AX, DX).encodeByte should be(Hex.lsb("66 EF"))
      }

      "correctly encode out dx, eax" in {
        Output(EAX, DX).encodeByte should be(Hex.lsb("EF"))
      }

    }
    "in long mode" should {

      import ProcessorMode.Long._

      "correctly encode out 0x10, al" in {
        Output(AL, 0x10.toByte).encodeByte should be(Hex.lsb("E6 10"))
      }

      "correctly encode out 0x20, ax" in {
        Output(AX, 0x20.toByte).encodeByte should be(Hex.lsb("66 E7 20"))
      }

      "correctly encode out 0x40, eax" in {
        Output(EAX, 0x40.toByte).encodeByte should be(Hex.lsb("E7 40"))
      }

      "correctly encode out dx, al" in {
        Output(AL, DX).encodeByte should be(Hex.lsb("EE"))
      }

      "correctly encode out dx, ax" in {
        Output(AX, DX).encodeByte should be(Hex.lsb("66 EF"))
      }

      "correctly encode out dx, eax" in {
        Output(EAX, DX).encodeByte should be(Hex.lsb("EF"))
      }
    }
  }
}