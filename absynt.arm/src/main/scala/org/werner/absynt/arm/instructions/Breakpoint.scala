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

import org.werner.absynt.arm.ProcessorMode
import org.werner.absynt.arm.operands.Condition._
import org.werner.absynt.arm.operations.Miscellaneous


object Breakpoint {
  trait A32Operations {
    object Breakpoint {
      val code: Byte = 0x09
      val opcode: String = "bkpt"

      def apply(value: Short, condition: Condition = Always)(implicit processorMode: ProcessorMode): Miscellaneous =
        Immed(value, condition)

      private def Immed(value: Short, condition: Condition) = new Miscellaneous(code, opcode, value, condition)
    }
  }
}
