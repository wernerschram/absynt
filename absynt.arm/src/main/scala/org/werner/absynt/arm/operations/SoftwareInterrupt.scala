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

package org.werner.absynt.arm.operations

import org.werner.absynt.arm.operands.Condition

class SoftwareInterrupt(override val opcode: String, interrupt: Int, condition: Condition)
  extends ARMOperation {
  override def encodeWord: Int = (condition.value << 28) | 0x0f000000 | interrupt

  override def toString: String = s"$mnemonicString ${interrupt.toString}"
}
