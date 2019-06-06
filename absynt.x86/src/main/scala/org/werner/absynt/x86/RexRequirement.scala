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

package org.werner.absynt.x86

sealed protected class RexRequirement private (val rexBitMask: Byte)

object RexRequirement {
  val instanceIndex = new RexRequirement(0x02)
  val instanceBase = new RexRequirement(0x01)
  val instanceOpcodeReg = new RexRequirement(0x01)
  val instanceOperandR = new RexRequirement(0x04)
  val instanceOperandRM = new RexRequirement(0x01)
  val quadOperand = new RexRequirement(0x08)
}
