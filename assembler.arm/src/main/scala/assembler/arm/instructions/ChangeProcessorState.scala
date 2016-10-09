package assembler.arm.instructions

import assembler.arm.ProcessorMode
import assembler.arm.opcodes.Effect
import assembler.arm.opcodes.ExecutionMode
import assembler.arm.opcodes.ProcessorState
import assembler.arm.opcodes.InterruptDisableFlags

object ChangeProcessorState {
  val code: Byte = 0x10
  val opcode: String = "cps"
  private val ProcessorState = new ProcessorState(code)(opcode)

    def apply(effect: Effect, interruptDisableFlags: InterruptDisableFlags.ValueSet, mode: ExecutionMode)(implicit processorMode: ProcessorMode) =
      ProcessorState(effect, interruptDisableFlags, mode)

    def apply(effect: Effect, interruptDisableFlags: InterruptDisableFlags.ValueSet)(implicit processorMode: ProcessorMode) =
      ProcessorState(effect, interruptDisableFlags)

    def apply(mode: ExecutionMode)(implicit processorMode: ProcessorMode) =
      ProcessorState(mode)
}