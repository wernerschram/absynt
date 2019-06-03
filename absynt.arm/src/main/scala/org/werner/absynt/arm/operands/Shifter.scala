package org.werner.absynt.arm.operands

import org.werner.absynt.arm.operands.registers.GeneralRegister
import org.werner.absynt.arm.operands.registers.GeneralRegister

import scala.annotation.tailrec
import scala.language.implicitConversions

trait ShiftValue {
  def encodeShiftValue: Int
}

trait LeftShiftValue extends ShiftValue

trait RightShiftValue extends ShiftValue

trait RotateValue extends ShiftValue

abstract class ImmediateShiftValue(val value: Byte) extends ShiftValue {
  override def toString = s"#$value"
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

  override def toString = s"$register, $mnemonic"
}

class ShiftRegisterWithShift[+T <: ShiftValue] private[operands](shifterCode: Int, mnemonic: String, register: GeneralRegister,
                                                                 shiftValue: T)
  extends ShiftRegister(shifterCode, mnemonic, register) {
  override val encode: Int = shifterCode | shiftValue.encodeShiftValue | register.registerCode

  override def toString = s"$register, $mnemonic $shiftValue"
}

case class RightRotateImmediate private[operands](immediate: Byte, rotateValue: Byte) extends Shifter {
  assume((rotateValue >= 0) && (rotateValue <= 30) && (rotateValue % 2 == 0))
  override val encode: Int = 0x02000000 | (rotateValue << 7) | (immediate & 0xff)

  override def toString = s"#$immediate, $rotateValue"
}

object Shifter {
  implicit def apply(register: GeneralRegister): Shifter = new Shifter() {
    override val encode: Int = register.registerCode.toInt

    override def toString = s"$register"
  }

  def LogicalLeftShift(register: GeneralRegister, shiftImmediate: Byte): ShiftRegisterWithShift[LeftImmediateShiftValue] =
    LogicalLeftShiftOperand(0x00, register, LeftImmediateShiftValue(shiftImmediate))

  def LogicalLeftShift(register: GeneralRegister, shiftRegister: GeneralRegister): ShiftRegisterWithShift[LeftShiftValue] =
    LogicalLeftShiftOperand(0x10, register, shiftRegister)

  private def LogicalLeftShiftOperand[T <: LeftShiftValue](shifterCode: Int, register: GeneralRegister, shift: T) =
    new ShiftRegisterWithShift(shifterCode, "lsl", register, shift)

  def LogicalRightShift(register: GeneralRegister, shiftImmediate: Byte): ShiftRegisterWithShift[RightImmediateShiftValue] =
    LogicalRightShiftOperand(0x20, register, RightImmediateShiftValue(shiftImmediate))

  private def LogicalRightShiftOperand[T <: RightShiftValue](shifterCode: Int, register: GeneralRegister, shift: T) =
    new ShiftRegisterWithShift(shifterCode, "lsr", register, shift)

  def LogicalRightShift(register: GeneralRegister, shiftRegister: GeneralRegister): ShiftRegisterWithShift[RightShiftValue] =
    LogicalRightShiftOperand(0x30, register, shiftRegister)

  def ArithmeticRightShift(register: GeneralRegister, shiftImmediate: Byte): ShiftRegisterWithShift[RightImmediateShiftValue] =
    ArithmeticRightShiftOperand(0x40, register, RightImmediateShiftValue(shiftImmediate))

  def ArithmeticRightShift(register: GeneralRegister, shiftRegister: GeneralRegister): ShiftRegisterWithShift[RightShiftValue] =
    ArithmeticRightShiftOperand(0x50, register, shiftRegister)

  private def ArithmeticRightShiftOperand[T <: RightShiftValue](shifterCode: Int, register: GeneralRegister, shift: T) =
    new ShiftRegisterWithShift(shifterCode, "asr", register, shift)

  def RightRotate(register: GeneralRegister, shiftImmediate: Byte): ShiftRegisterWithShift[ImmediateRotateValue] =
    RightRotateOperand(0x60, register, ImmediateRotateValue(shiftImmediate))

  def RightRotate(register: GeneralRegister, shiftRegister: GeneralRegister): ShiftRegisterWithShift[RotateValue] =
    RightRotateOperand(0x70, register, shiftRegister)

  private def RightRotateOperand[T <: RotateValue](shifterCode: Int, register: GeneralRegister, shift: T) =
    new ShiftRegisterWithShift(shifterCode, "ror", register, shift)

  def RightRotateExtend(register: GeneralRegister) =
    new ShiftRegister(0x60, "rrx", register)

  def RightRotateImmediate(immediate: Byte, rotateValue: Byte) =
    new RightRotateImmediate(immediate, rotateValue)

  implicit def RightRotateImmediate(immediate: Byte): RightRotateImmediate =
    new RightRotateImmediate(immediate, 0)

  implicit def ForImmediate(immediate: Int): RightRotateImmediate = {
    val rotateValue = (0 to 30 by 2).find { x => (Integer.rotateLeft(immediate, x) & 0xFF) == Integer.rotateLeft(immediate, x) }
    assume(rotateValue.isDefined)

    val rotate = rotateValue.get.toByte
    new RightRotateImmediate(Integer.rotateLeft(immediate, rotate).toByte, rotate)
  }

  private def CreateShifters(value: Int, minRotate: Int): Seq[RightRotateImmediate] = {
    if (value == 0)
      return Nil

    val shift = (minRotate to 30 by 2).find { x => ((value >>> x) & 0x03) != 0 }.get.toByte
    val intermediateValue = value >>> shift
    val rotate: Byte = if (shift == 0) 0 else (32 - shift).toByte

    Shifter.RightRotateImmediate((intermediateValue & 0xFF).toByte, rotate) +: CreateShifters(value & (0xFFFFFF00 << shift), shift)
  }

  implicit def apply(immediate: Int): Seq[RightRotateImmediate] = CreateShifters(immediate, 0)
}
