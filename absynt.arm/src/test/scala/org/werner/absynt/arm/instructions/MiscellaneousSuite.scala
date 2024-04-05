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

package org.werner.absynt.arm.instructions

import org.werner.absynt.Hex
import org.werner.absynt.arm.ProcessorMode
import org.werner.absynt.arm.operations.{Effect, ExecutionMode, InterruptDisableFlags}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scala.language.implicitConversions

class MiscellaneousSuite extends AnyWordSpec with Matchers {

  "a Breakpoint instruction" when {
    "in a32 mode" should {

      import ProcessorMode.A32.{given, *}

      "correctly encode bkpt 0x00cc" in {
        Breakpoint(0xf15.toShort).encodeByte should be(Hex.msb("e120f175"))
      }

      "correctly represent bkpt 3861 as a string" in {
        Breakpoint(0xf15.toShort).toString should be("bkpt 3861")
      }
    }
  }

  "a Change Processor State instruction" when {
    "in a32 mode" should {

      import ProcessorMode.A32.{given, *}

      "correctly encode cpsie , #31" in {
        ChangeProcessorState(Effect.InterruptEnable, InterruptDisableFlags.none, ExecutionMode.System).encodeByte should be(Hex.msb("f10a001f"))
      }

      "correctly represent cpsie , #31 as a string" in {
        ChangeProcessorState(Effect.InterruptEnable, InterruptDisableFlags.none, ExecutionMode.System).toString should be("cpsie , #31")
      }

      "correctly encode cpsid ai, #16" in {
        ChangeProcessorState(Effect.InterruptDisable, InterruptDisableFlags.impreciseDataAbort + InterruptDisableFlags.normalInterrupt, ExecutionMode.User).encodeByte should be(Hex.msb("f10e0190"))
      }

      "correctly encode cpsie f" in {
        ChangeProcessorState(Effect.InterruptEnable, InterruptDisableFlags.fastInterrupt).encodeByte should be(Hex.msb("f10a0040"))
      }

      "correctly represent cpsie f as a string" in {
        ChangeProcessorState(Effect.InterruptEnable, InterruptDisableFlags.fastInterrupt).toString should be("cpsie f")
      }

      "correctly encode cpsie if" in {
        ChangeProcessorState(Effect.InterruptEnable, InterruptDisableFlags.fastInterrupt + InterruptDisableFlags.normalInterrupt).encodeByte should be(Hex.msb("f10a00c0"))
      }

      "correctly represent cpsie if as a string" in {
        ChangeProcessorState(Effect.InterruptEnable, InterruptDisableFlags.fastInterrupt + InterruptDisableFlags.normalInterrupt).toString should be("cpsie if")
      }


      "correctly encode cps #19" in {
        ChangeProcessorState(ExecutionMode.Supervisor).encodeByte should be(Hex.msb("f1020013"))
      }
    }
  }
}