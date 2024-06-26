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

class LoadStoreMultipleDirection(val bitMask: Int)

object LoadStoreMultipleDirection {

  object Store extends LoadStoreMultipleDirection(0x00000000)
  object Load extends LoadStoreMultipleDirection(0x00100000)

}

class LoadStoreMultiple(direction: LoadStoreMultipleDirection, val condition: Condition, val registers:
                        Seq[GeneralRegister], val baseRegister: GeneralRegister, val addressingMode: UpdateMode, val opcode: String)
  extends Conditional {
  assume(registers.nonEmpty)
  assume(baseRegister != GeneralRegister.R15)

  override def encodeWord: Int =
    super.encodeWord | 0x08000000 |
      addressingMode.bitMask | direction.bitMask |
      (baseRegister.registerCode << 16) |
      registerBits

  val registerBits: Int =
    registers.foldLeft(0)((result, instance) => result | (1 << instance.registerCode))

  override def toString: String = s"$mnemonicString${addressingMode.mnemonicExtension} $baseRegisterString, $registerString"

  def baseRegisterString: String = baseRegister.toString()

  def registerString: String = s"{${registers.map { x => x.toString }.mkString(", ")}}"
}

trait UpdateBase extends LoadStoreMultiple {
  self: LoadStoreMultiple =>
  override def encodeWord: Int =
    super.encodeWord | 0x00200000

  override def baseRegisterString: String = s"${super.baseRegisterString}!"
}

trait UserModeRegisters extends LoadStoreMultiple {
  self: LoadStoreMultiple =>
  assume(!(this.isInstanceOf[UpdateBase] && !registers.contains(GeneralRegister.R15)))

  override def encodeWord: Int =
    super.encodeWord | 0x00400000

  override def registerString: String = s"${super.registerString}^"
}

class ReturnFromException(baseRegister: GeneralRegister, addressingMode: UpdateMode, updateBase: Boolean,
                          val opcode: String)
  extends ARMOperation {
  override def encodeWord: Int =
    0xf8100a00 |
      (if updateBase then 0x00200000 else 0) |
      addressingMode.bitMask |
      (baseRegister.registerCode << 16)

  override def toString: String = s"$mnemonicString${addressingMode.mnemonicExtension} $baseRegister${if updateBase then "!" else ""}"
}
