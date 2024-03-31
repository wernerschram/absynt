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
import org.werner.absynt.arm.operands.registers.GeneralRegister

class MultiplyOperation(val code: Byte, override val opcode: String, destination: GeneralRegister,
                        source: GeneralRegister, multiplyValue: GeneralRegister, val condition: Condition)
  extends Conditional {
  override def encodeWord: Int =
    super.encodeWord |
      0x00000090 |
      (code << 21) |
      (destination.registerCode << 16) |
      (source.registerCode << 8) |
      multiplyValue.registerCode

  override def toString: String = s"$mnemonicString ${destination.toString}, ${multiplyValue.toString}, ${source.toString}"
}

class MultiplyWithRegisterOperation(code: Byte, opcode: String, destination: GeneralRegister, source: GeneralRegister,
                                    multiplyValue: GeneralRegister, addValue: GeneralRegister, condition: Condition)
  extends MultiplyOperation(code, opcode, destination, source, multiplyValue, condition) {

  override def encodeWord: Int =
    super.encodeWord | (addValue.registerCode << 12)

  override def toString: String = s"${super.toString()}, ${addValue.toString}"
}
