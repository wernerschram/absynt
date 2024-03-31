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

import scala.language.implicitConversions

class Miscellaneous(val code: Byte, override val opcode: String, value: Short, condition: Condition)
  extends ARMOperation {
  override def encodeWord: Int = {
    val valuePart1: Byte = (value & 0x0f).toByte
    val valuePart2: Short = ((value & 0xfff0) >> 4).toShort
    val extraCode: Int = 0x7
    val result = (condition.value << 28) | (code << 21) | (valuePart2 << 8) | (extraCode << 4) | valuePart1
    result
  }

  override def toString = s"$mnemonicString $value"
}

sealed abstract class Effect(val iMod: Byte, val mnemonicExtension: String)

object Effect {

  case object InterruptEnable extends Effect(0x02, "ie")
  case object InterruptDisable extends Effect(0x03, "id")

}

sealed abstract class ExecutionMode(val mode: Byte) {
  override def toString: String = mode.toString
}

object ExecutionMode {

  case object User extends ExecutionMode(0x10)
  case object FastInterruptRequest extends ExecutionMode(0x11)
  case object NormalInterruptRequest extends ExecutionMode(0x12)
  case object Supervisor extends ExecutionMode(0x13)
  case object Abort extends ExecutionMode(0x17)
  case object Undefined extends ExecutionMode(0x03)
  case object System extends ExecutionMode(0x1f)

}

object InterruptDisableFlags extends Enumeration {
  type InterruptDisableFlags = Value

  val fastInterrupt: Value = Value(0, "f")
  val normalInterrupt: Value = Value(1, "i")
  val impreciseDataAbort: Value = Value(2, "a")

  val none: ValueSet = ValueSet.empty

  implicit def valueToSet(value: Value): ValueSet = ValueSet(value)

  implicit def flagsToString(set: ValueSet): String =
    set.foldRight(new StringBuilder)((value, builder) => builder.addAll(value.toString)).result()
}

class ProcessorState(val code: Byte, val opcode: String, val condition: Condition,
                     iMod: Byte, mMod: Byte, iflags: Int, modeValue: Int, postFixString: String)
  extends Conditional {

  def this(code: Byte, opcode: String, mode: ExecutionMode) =
    this(code, opcode, Condition.Unpredictable, 0x00.toByte, 0x01.toByte, 0x00, mode.mode, s" #$mode")

  def this(code: Byte, opcode: String, effect: Effect, interruptDisableFlags: InterruptDisableFlags.ValueSet) =
    this(code: Byte, opcode: String, Condition.Unpredictable, effect.iMod, 0x01.toByte,
      interruptDisableFlags.toBitMask(0).toInt << 6, 0x00,
      s"${effect.mnemonicExtension} ${InterruptDisableFlags.flagsToString(interruptDisableFlags)}")

  def this(code: Byte, opcode: String, effect: Effect, interruptDisableFlags: InterruptDisableFlags.ValueSet,
           mode: ExecutionMode) =
    this(code, opcode, Condition.Unpredictable, effect.iMod, 0x01.toByte,
      interruptDisableFlags.toBitMask(0).toInt << 6, mode.mode,
      s"${effect.mnemonicExtension} ${InterruptDisableFlags.flagsToString(interruptDisableFlags)}, #$mode")

  override def encodeWord: Int =
    super.encodeWord | (code << 20) | (iMod << 18) | (mMod << 17) | iflags | modeValue

  override def toString = s"$mnemonicString$postFixString"
}