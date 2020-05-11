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
import org.werner.absynt.x86.operands.ReturnMode
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SystemSuite extends AnyWordSpec with Matchers {

  "a SysCall instruction" when {

    "in protected mode" should {
      import ProcessorMode.Protected._


      "correctly encode sysenter" in {
        SystemEnter().encodeByte should be (Hex.lsb("0F 34"))
      }

      "correctly encode sysexit to protected mode" in {
        SystemExit(ReturnMode.Protected).encodeByte should be (Hex.lsb("0F 35"))
      }

    }

    "in long mode" should {

      import ProcessorMode.Long._

      "correctly encode syscall" in {
        SystemCall().encodeByte should be (Hex.lsb("0F 05"))
      }

      "correctly represent syscall as a string" in {
        SystemCall().toString should be("syscall")
      }

      "correctly encode sysenter" in {
        SystemEnter().encodeByte should be (Hex.lsb("0F 34"))
      }

      "correctly represent sysenter as a string" in {
        SystemEnter().toString should be("sysenter")
      }

      "correctly encode sysexit to protected mode" in {
        SystemExit(ReturnMode.Protected).encodeByte should be (Hex.lsb("0F 35"))
      }

      "correctly encode sysexit to long mode" in {
        SystemExit(ReturnMode.Long).encodeByte should be (Hex.lsb("48 0F 35"))
      }

      "correctly represent sysexit as a string" in {
        SystemExit(ReturnMode.Protected).toString should be("sysexit")
      }

      "correctly encode sysret to protected mode" in {
        SystemReturn(ReturnMode.Protected).encodeByte should be (Hex.lsb("0F 07"))
      }

      "correctly encode sysret to long mode" in {
        SystemReturn(ReturnMode.Long).encodeByte should be (Hex.lsb("48 0F 07"))
      }

      "correctly represent sysret as a string" in {
        SystemReturn(ReturnMode.Protected).toString should be("sysret")
      }
    }
  }
}