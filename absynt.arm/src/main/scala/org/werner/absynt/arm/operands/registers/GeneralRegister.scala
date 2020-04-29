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

package org.werner.absynt.arm.operands.registers

import org.werner.absynt.arm.operands._

abstract class Register(mnemonic: String) extends Operand {
  override def toString: String = mnemonic
}

sealed abstract class GeneralRegister(val registerCode: Byte) extends Register(s"r$registerCode")
  with LeftShiftValue with RightShiftValue with RotateValue {
  val shifter: Short = registerCode.toShort

  override def encodeShiftValue: Int = registerCode << 8
}

private[arm] object GeneralRegister {
  case object R0 extends GeneralRegister(0x00)
  case object R1 extends GeneralRegister(0x01)
  case object R2 extends GeneralRegister(0x02)
  case object R3 extends GeneralRegister(0x03)
  case object R4 extends GeneralRegister(0x04)
  case object R5 extends GeneralRegister(0x05)
  case object R6 extends GeneralRegister(0x06)
  case object R7 extends GeneralRegister(0x07)
  case object R8 extends GeneralRegister(0x08)
  case object R9 extends GeneralRegister(0x09)
  case object R10 extends GeneralRegister(0x0a)
  case object R11 extends GeneralRegister(0x0b)
  case object R12 extends GeneralRegister(0x0c)
  case object R13 extends GeneralRegister(0x0d)
  case object R14 extends GeneralRegister(0x0e)
  case object R15 extends GeneralRegister(0x0f)
}

sealed abstract class StatusRegister(val registerCode: Byte, mnemonic: String) extends Register(mnemonic) {
  val shifter: Short = registerCode.toShort
}

object StatusRegister {

  case object CPSR extends StatusRegister(0x00, "CPSR")
  case object SPSR extends StatusRegister(0x01, "SPSR")

}

object Register {
  trait ARMRegisters {
    val R0: GeneralRegister = GeneralRegister.R0
    val R1: GeneralRegister = GeneralRegister.R1
    val R2: GeneralRegister = GeneralRegister.R2
    val R3: GeneralRegister = GeneralRegister.R3
    val R4: GeneralRegister = GeneralRegister.R4
    val R5: GeneralRegister = GeneralRegister.R5
    val R6: GeneralRegister = GeneralRegister.R6
    val R7: GeneralRegister = GeneralRegister.R7
    val R8: GeneralRegister = GeneralRegister.R8
    val R9: GeneralRegister = GeneralRegister.R9
    val R10: GeneralRegister = GeneralRegister.R10
    val R11: GeneralRegister = GeneralRegister.R11
    val R12: GeneralRegister = GeneralRegister.R12
    val R13: GeneralRegister = GeneralRegister.R13
    val R14: GeneralRegister = GeneralRegister.R14
    val R15: GeneralRegister = GeneralRegister.R15

    val SP: GeneralRegister = R13
    val LR: GeneralRegister = R14
    val PC: GeneralRegister = R15

    val CPSR: StatusRegister = StatusRegister.CPSR
    val SPSR: StatusRegister = StatusRegister.SPSR
  }
}