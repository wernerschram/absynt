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

import org.werner.absynt.arm.operations.{Effect, ExecutionMode, InterruptDisableFlags, ProcessorState}

object ChangeProcessorState {

  trait A32Operations {

    object ChangeProcessorState {
      val code: Byte = 0x10
      val opcode: String = "cps"

      def apply(effect: Effect, interruptDisableFlags: InterruptDisableFlags.ValueSet, mode: ExecutionMode): ProcessorState =
        ProcessorState(effect, interruptDisableFlags, mode)

      private def ProcessorState(effect: Effect, interruptDisableFlags: InterruptDisableFlags.ValueSet, mode: ExecutionMode) =
        new ProcessorState(code, opcode, effect, interruptDisableFlags, mode)

      def apply(effect: Effect, interruptDisableFlags: InterruptDisableFlags.ValueSet): ProcessorState =
        ProcessorState(effect, interruptDisableFlags)

      private def ProcessorState(effect: Effect, interruptDisableFlags: InterruptDisableFlags.ValueSet) =
        new ProcessorState(code, opcode, effect, interruptDisableFlags)

      def apply(mode: ExecutionMode): ProcessorState =
        ProcessorState(mode)

      private def ProcessorState(mode: ExecutionMode) =
        new ProcessorState(code, opcode, mode)
    }

  }

}
