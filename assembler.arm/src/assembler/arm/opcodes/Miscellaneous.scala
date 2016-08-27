package assembler.arm.opcodes

import assembler.ListExtensions._
import assembler.arm.operands.Operand
import assembler.arm.operands.registers.GeneralRegister
import assembler.arm.operands.Condition._
import assembler.arm.ProcessorMode
import assembler.arm.instructions.ARMInstruction
import assembler.MemoryPage
import assembler.arm.operands.Shifter
import assembler.arm.instructions.ConditionalARMInstruction

class Miscellaneous(val code: Byte)(implicit mnemonic: String)
    extends Opcode(mnemonic) {

  def apply(value: Short, condition: Condition)(implicit processorMode: ProcessorMode): ARMInstruction = {
    new ARMInstruction() {
      override def encodeWord()(implicit page: MemoryPage) = { 
        val valuePart1: Byte = (value & 0x0f).toByte
        val valuePart2: Short = ((value & 0xfff0) >> 4).toShort
        val extraCode: Int = 0x7
        val result = ((condition.value << 28) | (code << 21) | (valuePart2 << 8) | (extraCode << 4) | valuePart1)
        result
      }
      
      override def toString() = s"${Miscellaneous.this.mnemonic}" // ${value.toString()}}"
    }
  }
}

sealed abstract class Effect(val iMod: Byte, val mnemonicExtension: String)

object Effect {
  case object InterruptEnable extends Effect(0x02, "ie")
  case object InterruptDisable extends Effect(0x03, "id")
}

sealed abstract class ExecutionMode(val mode: Byte) {
  override val toString = mode.toString()
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

class ProcessorState(val code: Byte)(implicit mnemonic: String) {

  def iflags(impreciseDataAbort: Boolean, normalInterrupt: Boolean, fastInterrupt: Boolean): String =
    ((if (impreciseDataAbort) "a" else "") +
      (if (normalInterrupt) "i" else "") +
      (if (fastInterrupt) "f" else ""))

  private def apply(condition: Condition, iMod: Byte, mMod: Byte, iflags: Int, modeValue: Byte, stringValue: String)(implicit processorMode: ProcessorMode): ARMInstruction =
    new ConditionalARMInstruction(condition) {
      override def encodeWord()(implicit page: MemoryPage) = 
        (super.encodeWord() | (code << 20) | (iMod << 18) | (mMod << 17) | iflags | modeValue)

      override def toString() = stringValue
    }

  def apply(effect: Effect, impreciseDataAbort: Boolean, normalInterrupt: Boolean, fastInterrupt: Boolean, mode: ExecutionMode)(implicit processorMode: ProcessorMode): ARMInstruction =
    apply(Unpredictable, effect.iMod, 0x01.toByte,
      ((if (impreciseDataAbort) 0x100 else 0) |
        (if (normalInterrupt) 0x80 else 0) |
        (if (fastInterrupt) 0x40 else 0)), mode.mode,
      s"${ProcessorState.this.mnemonic}${effect.mnemonicExtension} ${iflags(impreciseDataAbort, normalInterrupt, fastInterrupt)}, #${mode}")

  def apply(effect: Effect, impreciseDataAbort: Boolean, normalInterrupt: Boolean, fastInterrupt: Boolean)(implicit processorMode: ProcessorMode): ARMInstruction =
    apply(
      Unpredictable,
      effect.iMod,
      0x00.toByte,
      ((if (impreciseDataAbort) 0x100 else 0) |
        (if (normalInterrupt) 0x80 else 0) |
        (if (fastInterrupt) 0x40 else 0)),
      0x00,
      s"${ProcessorState.this.mnemonic}${effect.mnemonicExtension} ${iflags(impreciseDataAbort, normalInterrupt, fastInterrupt)}")

  def apply(mode: ExecutionMode)(implicit processorMode: ProcessorMode): ARMInstruction =
    apply(Unpredictable,
      0x00.toByte,
      0x01.toByte,
      0x00,
      mode.mode,
      s"${ProcessorState.this.mnemonic} #${mode}")
}