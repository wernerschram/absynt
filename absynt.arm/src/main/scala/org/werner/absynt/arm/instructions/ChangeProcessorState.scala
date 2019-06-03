package org.werner.absynt.arm.instructions

import org.werner.absynt.arm.ProcessorMode
import org.werner.absynt.arm.operations.{Effect, ExecutionMode, InterruptDisableFlags, ProcessorState}

object ChangeProcessorState {
  val code: Byte = 0x10
  val opcode: String = "cps"

  def apply(effect: Effect, interruptDisableFlags: InterruptDisableFlags.ValueSet, mode: ExecutionMode)
           (implicit processorMode: ProcessorMode): ProcessorState =
    ProcessorState(effect, interruptDisableFlags, mode)

  private def ProcessorState(effect: Effect, interruptDisableFlags: InterruptDisableFlags.ValueSet, mode: ExecutionMode) =
    new ProcessorState(code, opcode, effect, interruptDisableFlags, mode)

  def apply(effect: Effect, interruptDisableFlags: InterruptDisableFlags.ValueSet)(implicit processorMode: ProcessorMode): ProcessorState =
    ProcessorState(effect, interruptDisableFlags)

  private def ProcessorState(effect: Effect, interruptDisableFlags: InterruptDisableFlags.ValueSet) =
    new ProcessorState(code, opcode, effect, interruptDisableFlags)

  def apply(mode: ExecutionMode)(implicit processorMode: ProcessorMode): ProcessorState =
    ProcessorState(mode)

  private def ProcessorState(mode: ExecutionMode) =
    new ProcessorState(code, opcode, mode)
}