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
import org.werner.absynt.arm.operations.UpdateMode
import org.scalatest.{Matchers, WordSpec}

class LoadStoreMultipleSuite extends WordSpec with Matchers {

  "a LoadMultiple instruction" when {
    "in a32 mode" should {

      import ProcessorMode.A32._

      "correctly encode ldm r4, {r1, r2}" in {
        LoadMultiple(R1 :: R2 :: Nil, R4, UpdateMode.IncrementAfter).encodeByte should be(Hex.msb("e8940006"))
      }

      "correctly represent ldm r4, {r1, r2} as a string" in {
        LoadMultiple(R1 :: R2 :: Nil, R4, UpdateMode.IncrementAfter).toString should be("ldm r4, {r1, r2}")
      }

      "correctly encode ldm r1!, {r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14}" in {
        LoadMultiple.withUpdateBase(R1 :: R2 :: R3 :: R4 :: R5 :: R6 :: R7 :: R8 :: R9 :: R10 :: R11 :: R12 :: R13 :: R14 :: Nil, R1, UpdateMode.IncrementAfter).encodeByte should be(Hex.msb("e8b17ffe"))
      }

      "correctly encode ldm r1, {r14}^" in {
        LoadMultiple.withUserModeRegisters(R14 :: Nil, R1, UpdateMode.IncrementAfter).encodeByte should be(Hex.msb("e8d14000"))
      }

      "correctly represent ldm r1, {r14}^ as a string" in {
        LoadMultiple.withUserModeRegisters(R14 :: Nil, R1, UpdateMode.IncrementAfter).toString should be("ldm r1, {r14}^")
      }

      "correctly encode ldm r1!, {r15, r14}^" in {
        LoadMultiple.withUserModeRegistersAndUpdateBase(R15 :: R14 :: Nil, R1, UpdateMode.IncrementAfter).encodeByte should be(Hex.msb("e8f1c000"))
      }

      "correctly encode push lr" in {
        Push(LR :: Nil).encodeByte should be(Hex.msb("e83d4000"))
      }

      "throw an AssertionError for ldm r1!, {r14}^" in {
        an[AssertionError] should be thrownBy {
          LoadMultiple.withUserModeRegistersAndUpdateBase(R14 :: Nil, R1, UpdateMode.IncrementAfter)
        }
      }
    }
  }

  "a StoreMultiple instruction" when {
    "in a32 mode" should {

      import ProcessorMode.A32._

      "correctly encode stm r4, {r1, r2}" in {
        StoreMultiple(R1 :: R2 :: Nil, R4, UpdateMode.IncrementAfter).encodeByte should be(Hex.msb("e8840006"))
      }

      "correctly encode stmib r1!, {r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14}" in {
        StoreMultiple.withUpdateBase(R1 :: R2 :: R3 :: R4 :: R5 :: R6 :: R7 :: R8 :: R9 :: R10 :: R11 :: R12 :: R13 :: R14 :: Nil, R1, UpdateMode.IncrementBefore).encodeByte should be(Hex.msb("e9a17ffe"))
      }

      "correctly represent stmib r1!, {r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14} as a string" in {
        StoreMultiple.withUpdateBase(R1 :: R2 :: R3 :: R4 :: R5 :: R6 :: R7 :: R8 :: R9 :: R10 :: R11 :: R12 :: R13 :: R14 :: Nil, R1, UpdateMode.IncrementBefore).toString should be("stmib r1!, {r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14}")
      }

      "correctly encode stm r1, {r14}^" in {
        StoreMultiple.withUserModeRegisters(R14 :: Nil, R1, UpdateMode.DecrementBefore).encodeByte should be(Hex.msb("e9414000"))
      }

      "correctly encode pop r1, sp, pc" in {
        Pop(R1 :: SP :: PC :: Nil).encodeByte should be(Hex.msb("e9ada002"))
      }
    }
  }

  "a ReturnFromException instruction" when {
    "in a32 mode" should {

      import ProcessorMode.A32._

      "correctly encode rfeia r5" in {
        ReturnFromException(R5, UpdateMode.IncrementAfter).encodeByte should be(Hex.msb("f8950a00"))
      }

      "correctly represent rfe r5 as a string" in {
        ReturnFromException(R5, UpdateMode.IncrementAfter).toString should be("rfe r5")
      }

      "correctly encode rfedb r5!" in {
        ReturnFromException.withUpdateBase(R5, UpdateMode.DecrementBefore).encodeByte should be(Hex.msb("f9350a00"))
      }

      "correctly represent rfedb r5! as a string" in {
        ReturnFromException.withUpdateBase(R5, UpdateMode.DecrementBefore).toString should be("rfedb r5!")
      }

    }
  }
}