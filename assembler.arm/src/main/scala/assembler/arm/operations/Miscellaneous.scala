package assembler.arm.operations

import assembler.arm.operands.Condition._
import assembler.memory.MemoryPage

import scala.language.implicitConversions

class Miscellaneous(val code: Byte, override val opcode: String, value: Short, condition: Condition)
    extends ARMOperation {
  override def encodeWord()(implicit page: MemoryPage): Int = {
    val valuePart1: Byte = (value & 0x0f).toByte
    val valuePart2: Short = ((value & 0xfff0) >> 4).toShort
    val extraCode: Int = 0x7
    val result = (condition.value << 28) | (code << 21) | (valuePart2 << 8) | (extraCode << 4) | valuePart1
    result
  }

  override def toString = s"${super.toString()} $value"
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

  val fastInterrupt = Value(0, "f")
  val normalInterrupt = Value(1, "i")
  val impreciseDataAbort = Value(2, "a")

  val none: _root_.assembler.arm.operations.InterruptDisableFlags.ValueSet = ValueSet.empty

  implicit def valueToSet(value: Value): ValueSet = ValueSet(value)

  implicit def flagsToString(set: ValueSet): String = {
    set.foldRight("")((a, b) => a + b).reverse
  }
}

class ProcessorState(val code: Byte, val opcode: String, val condition: Condition, iMod: Byte, mMod: Byte, iflags: Int, modeValue: Int, stringValue: String)
    extends Conditional {

  def this(code: Byte, opcode: String, mode: ExecutionMode) =
    this(code, opcode, Unpredictable, 0x00.toByte, 0x01.toByte, 0x00, mode.mode, s" #$mode")

  def this(code: Byte, opcode: String, effect: Effect, interruptDisableFlags: InterruptDisableFlags.ValueSet) =
    this(code: Byte, opcode: String, Unpredictable, effect.iMod, 0x01.toByte,
      interruptDisableFlags.toBitMask (0).toInt << 6, 0x00,
      s"${effect.mnemonicExtension} ${InterruptDisableFlags.flagsToString(interruptDisableFlags)}")

  def this(code: Byte, opcode: String, effect: Effect, interruptDisableFlags: InterruptDisableFlags.ValueSet, mode: ExecutionMode) =
    this(code, opcode, Unpredictable, effect.iMod, 0x01.toByte,
      interruptDisableFlags.toBitMask (0).toInt << 6, mode.mode,
      s"${effect.mnemonicExtension} ${InterruptDisableFlags.flagsToString(interruptDisableFlags)}, #$mode")

  override def encodeWord()(implicit page: MemoryPage): Int =
    super.encodeWord() | (code << 20) | (iMod << 18) | (mMod << 17) | iflags | modeValue

  override def toString = s"${super.toString()}$stringValue"
}