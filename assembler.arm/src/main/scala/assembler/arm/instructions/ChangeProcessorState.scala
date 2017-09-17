package assembler.arm.instructions

import assembler.Label
import assembler.arm.ProcessorMode
import assembler.arm.operations.{Effect, ExecutionMode, InterruptDisableFlags, ProcessorState}

object ChangeProcessorState {
  val code: Byte = 0x10
  val opcode: String = "cps"

  def apply(effect: Effect, interruptDisableFlags: InterruptDisableFlags.ValueSet, mode: ExecutionMode)
           (implicit label: Label, processorMode: ProcessorMode): ProcessorState =
    ProcessorState(label, effect, interruptDisableFlags, mode)

  private def ProcessorState(label: Label, effect: Effect, interruptDisableFlags: InterruptDisableFlags.ValueSet, mode: ExecutionMode) =
    new ProcessorState(label, code, opcode, effect, interruptDisableFlags, mode)

  def apply(effect: Effect, interruptDisableFlags: InterruptDisableFlags.ValueSet)(implicit label: Label, processorMode: ProcessorMode): ProcessorState =
    ProcessorState(label, effect, interruptDisableFlags)

  private def ProcessorState(label: Label, effect: Effect, interruptDisableFlags: InterruptDisableFlags.ValueSet) =
    new ProcessorState(label, code, opcode, effect, interruptDisableFlags)

  def apply(mode: ExecutionMode)(implicit label: Label, processorMode: ProcessorMode): ProcessorState =
    ProcessorState(label, mode)

  private def ProcessorState(label: Label, mode: ExecutionMode) =
    new ProcessorState(label, code, opcode, mode)
}