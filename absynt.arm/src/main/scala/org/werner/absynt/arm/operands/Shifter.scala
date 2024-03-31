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

package org.werner.absynt.arm.operands

import org.werner.absynt.arm.operands.registers.GeneralRegister

import scala.language.implicitConversions

trait ShiftValue {
  def encodeShiftValue: Int
}

trait LeftShiftValue extends ShiftValue

trait RightShiftValue extends ShiftValue

trait RotateValue extends ShiftValue

abstract class ImmediateShiftValue(val value: Byte) extends ShiftValue {
  override def toString: String = s"#$value"
}

class LeftImmediateShiftValue private[operands](value: Byte) extends ImmediateShiftValue(value) with LeftShiftValue {
  assume(value >= 0 && value <= 31)

  override def encodeShiftValue: Int = value << 7
}

class RightImmediateShiftValue private[operands](value: Byte) extends ImmediateShiftValue(value) with RightShiftValue {
  assume(value >= 1 && value <= 32)

  //  override def encodeShiftValue = (value match { case 32 => 0; case default => value }) << 7
  override def encodeShiftValue: Int = (value & 0x1F) << 7
}

class ImmediateRotateValue private[operands](value: Byte) extends ImmediateShiftValue(value) with RotateValue {
  assume(value >= 1 && value <= 31)

  override def encodeShiftValue: Int = value << 7
}

object LeftImmediateShiftValue {
  def apply(value: Byte) = new LeftImmediateShiftValue(value)
}

object RightImmediateShiftValue {
  def apply(value: Byte) = new RightImmediateShiftValue(value)
}

object ImmediateRotateValue {
  def apply(value: Byte) = new ImmediateRotateValue(value)
}

abstract class Shifter {
  def encode: Int
}

class ShiftRegister private[operands](shifterCode: Int, mnemonic: String, register: GeneralRegister) extends Shifter {
  override val encode: Int = shifterCode | register.registerCode

  override def toString: String = s"$register, $mnemonic"
}

class ShiftRegisterWithShift[+T <: ShiftValue] private[operands](shifterCode: Int, mnemonic: String, register: GeneralRegister,
                                                                 shiftValue: T)
  extends ShiftRegister(shifterCode, mnemonic, register) {
  override val encode: Int = shifterCode | shiftValue.encodeShiftValue | register.registerCode

  override def toString: String = s"$register, $mnemonic $shiftValue"
}

case class RightRotateImmediate private[operands](immediate: Byte, rotateValue: Byte) extends Shifter {
  assume((rotateValue >= 0) && (rotateValue <= 30) && (rotateValue % 2 == 0))
  override val encode: Int = 0x02000000 | (rotateValue << 7) | (immediate & 0xff)

  override def toString: String = s"#$immediate, $rotateValue"
}

object Shifter {
  trait A32Shifter {
    implicit def shifter(register: GeneralRegister): Shifter = new Shifter() {
      override val encode: Int = register.registerCode.toInt

      override def toString = s"$register"
    }

    object Shift {
      private def logicalLeftShiftOperand[T <: LeftShiftValue](shifterCode: Int, register: GeneralRegister, shift: T) =
        new ShiftRegisterWithShift(shifterCode, "lsl", register, shift)

      def logicalLeftShift(register: GeneralRegister, shiftImmediate: Byte): ShiftRegisterWithShift[LeftImmediateShiftValue] =
        logicalLeftShiftOperand(0x00, register, LeftImmediateShiftValue(shiftImmediate))

      def logicalLeftShift(register: GeneralRegister, shiftRegister: GeneralRegister): ShiftRegisterWithShift[LeftShiftValue] =
        logicalLeftShiftOperand(0x10, register, shiftRegister)


      private def logicalRightShiftOperand[T <: RightShiftValue](shifterCode: Int, register: GeneralRegister, shift: T) =
        new ShiftRegisterWithShift(shifterCode, "lsr", register, shift)

      def logicalRightShift(register: GeneralRegister, shiftImmediate: Byte): ShiftRegisterWithShift[RightImmediateShiftValue] =
        logicalRightShiftOperand(0x20, register, RightImmediateShiftValue(shiftImmediate))

      def LogicalRightShift(register: GeneralRegister, shiftRegister: GeneralRegister): ShiftRegisterWithShift[RightShiftValue] =
        logicalRightShiftOperand(0x30, register, shiftRegister)


      private def arithmeticRightShiftOperand[T <: RightShiftValue](shifterCode: Int, register: GeneralRegister, shift: T) =
        new ShiftRegisterWithShift(shifterCode, "asr", register, shift)

      def arithmeticRightShift(register: GeneralRegister, shiftImmediate: Byte): ShiftRegisterWithShift[RightImmediateShiftValue] =
        arithmeticRightShiftOperand(0x40, register, RightImmediateShiftValue(shiftImmediate))

      def arithmeticRightShift(register: GeneralRegister, shiftRegister: GeneralRegister): ShiftRegisterWithShift[RightShiftValue] =
        arithmeticRightShiftOperand(0x50, register, shiftRegister)


      private def rightRotateOperand[T <: RotateValue](shifterCode: Int, register: GeneralRegister, shift: T) =
        new ShiftRegisterWithShift(shifterCode, "ror", register, shift)

      def rightRotate(register: GeneralRegister, shiftImmediate: Byte): ShiftRegisterWithShift[ImmediateRotateValue] =
        rightRotateOperand(0x60, register, ImmediateRotateValue(shiftImmediate))

      def rightRotate(register: GeneralRegister, shiftRegister: GeneralRegister): ShiftRegisterWithShift[RotateValue] =
        rightRotateOperand(0x70, register, shiftRegister)


      def rightRotateExtend(register: GeneralRegister) =
        new ShiftRegister(0x60, "rrx", register)

      def rightRotateImmediate(immediate: Byte, rotateValue: Byte): RightRotateImmediate =
        RightRotateImmediate(immediate, rotateValue)

      def rightRotateImmediate(immediate: Byte): RightRotateImmediate =
        RightRotateImmediate(immediate, 0)
    }

    implicit def rightRotateImmediate(immediate: Byte): RightRotateImmediate =
      RightRotateImmediate(immediate, 0)

    implicit def shifterForImmediate(immediate: Int): RightRotateImmediate = {
      val rotateValue = Range.inclusive(0, 30, 2).find { x => (Integer.rotateLeft(immediate, x) & 0xFF) == Integer.rotateLeft(immediate, x) }
      assume(rotateValue.isDefined)

      val rotate = rotateValue.get.toByte
      RightRotateImmediate(Integer.rotateLeft(immediate, rotate).toByte, rotate)
    }

    private def immediateShifters(value: Int, minRotate: Int): Seq[RightRotateImmediate] = {
      if (value == 0)
        return Nil

      val shift = Range.inclusive(minRotate, 30, 2).find { x => ((value >>> x) & 0x03) != 0 }.get.toByte
      val intermediateValue = value >>> shift
      val rotate: Byte = if (shift == 0) 0 else (32 - shift).toByte

      RightRotateImmediate((intermediateValue & 0xFF).toByte, rotate) +:  immediateShifters(value & (0xFFFFFF00 << shift), shift)
    }

    implicit def immediateShifter(immediate: Int): Seq[RightRotateImmediate] =  immediateShifters(immediate, 0)
  }
}
