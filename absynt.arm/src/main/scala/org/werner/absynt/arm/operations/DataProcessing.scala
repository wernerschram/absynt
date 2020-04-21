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
import org.werner.absynt.arm.operands.Shifter
import org.werner.absynt.arm.operands.registers.GeneralRegister

class DataProcessingOperation(val opcode: String, code: Byte, val condition: Condition, register1: GeneralRegister,
                              operand2: Shifter, destination: GeneralRegister)
  extends Conditional {
  override def encodeWord: Int =
    super.encodeWord | (code << 21) | (register1.registerCode << 16) | (destination.registerCode << 12) | operand2.encode

  override def toString = s"$mnemonicString ${destination.toString}, ${register1.toString}, ${operand2.toString}"
}

class DataProcessingNoDestinationInstruction(val opcode: String, code: Byte, val condition: Condition,
                                             register1: GeneralRegister, operand2: Shifter)
  extends Conditional {
  override def encodeWord: Int =
    super.encodeWord | 0x00100000 | (code << 21) | (register1.registerCode << 16) | operand2.encode

  override def toString = s"$mnemonicString ${register1.toString}, ${operand2.toString}"
}

class DataProcessingNoRegisterInstruction(val opcode: String, code: Byte, val condition: Condition, operand2: Shifter,
                                          destination: GeneralRegister)
  extends Conditional {
  override def encodeWord: Int =
    super.encodeWord | (code << 21) | (destination.registerCode << 12) | operand2.encode

  override def toString = s"$mnemonicString ${destination.toString}, ${operand2.toString}"
}
