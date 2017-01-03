package assembler.arm.instructions

import assembler.arm.ProcessorMode
import assembler.arm.operations.{Effect, ExecutionMode, InterruptDisableFlags, ProcessorState}

object ChangeProcessorState {
  val code: Byte = 0x10
  val opcode: String = "cps"

  private def ProcessorState(mode: ExecutionMode) =
    new ProcessorState(code, opcode, mode)

  private def ProcessorState(effect: Effect, interruptDisableFlags: InterruptDisableFlags.ValueSet) =
    new ProcessorState(code, opcode, effect, interruptDisableFlags)

  private def ProcessorState(effect: Effect, interruptDisableFlags: InterruptDisableFlags.ValueSet, mode: ExecutionMode) =
    new ProcessorState(code, opcode, effect, interruptDisableFlags, mode)

    def apply(effect: Effect, interruptDisableFlags: InterruptDisableFlags.ValueSet, mode: ExecutionMode)(implicit processorMode: ProcessorMode) =
      ProcessorState(effect, interruptDisableFlags, mode)

    def apply(effect: Effect, interruptDisableFlags: InterruptDisableFlags.ValueSet) =
      ProcessorState(effect, interruptDisableFlags)

    def apply(mode: ExecutionMode) =
      ProcessorState(mode)
}