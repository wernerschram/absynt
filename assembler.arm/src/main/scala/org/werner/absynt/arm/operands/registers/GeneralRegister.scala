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

object GeneralRegister {
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

  val SP: GeneralRegister = R13
  val LR: GeneralRegister = R14
  val PC: GeneralRegister = R15
}

sealed abstract class StatusRegister(val registerCode: Byte, mnemonic: String) extends Register(mnemonic) {
  val shifter: Short = registerCode.toShort
}

object StatusRegister {

  case object CPSR extends StatusRegister(0x00, "CPSR")
  case object SPSR extends StatusRegister(0x01, "SPSR")

}