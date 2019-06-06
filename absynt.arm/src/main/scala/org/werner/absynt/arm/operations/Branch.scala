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

import org.werner.absynt.arm.operands.Condition._
import org.werner.absynt.arm.operands.registers.GeneralRegister
import org.werner.absynt.arm.operands.{RelativeA32Pointer, RelativePointer, RelativeThumbPointer}

class BranchImmediate[AddressType<:RelativePointer](destination: AddressType, val condition: Condition, val code: Byte, val opcode: String)
  extends Conditional {
  override def encodeWord: Int =
    super.encodeWord | ((code & 0xF0) << 20) | destination.encode

  override def toString: String = {
    destination match {
      case p: RelativeA32Pointer =>
        s"$mnemonicString ${p.toString}"
      case p: RelativeThumbPointer =>
        s"$mnemonicString ${p.toString}"
    }
  }
}

class BranchRegister(destination: GeneralRegister, val condition: Condition, val code: Byte, val opcode: String)
  extends Conditional {
  override def encodeWord: Int =
    super.encodeWord | 0x012FFF00 | ((code & 0x0F) << 4) | destination.registerCode

  override def toString = s"$mnemonicString ${destination.toString}"
}
