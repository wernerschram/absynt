package assembler.arm.instructions

import assembler.arm.ProcessorMode
import assembler.arm.opcodes.ProcessorState
import assembler.arm.opcodes.ExecutionMode
import assembler.arm.opcodes.Effect

object ChangeProcessorState {
  val code: Byte = 0x10
  val opcode: String = "cps"
  private val ProcessorState = new ProcessorState(code)(opcode)

    def apply(effect: Effect, impreciseDataAbort: Boolean, normalInterrupt: Boolean, fastInterrupt: Boolean, mode: ExecutionMode)(implicit processorMode: ProcessorMode) =
      ProcessorState(effect, impreciseDataAbort, normalInterrupt, fastInterrupt, mode)

    def apply(effect: Effect, impreciseDataAbort: Boolean, normalInterrupt: Boolean, fastInterrupt: Boolean)(implicit processorMode: ProcessorMode) =
      ProcessorState(effect, impreciseDataAbort, normalInterrupt, fastInterrupt)

    def apply(mode: ExecutionMode)(implicit processorMode: ProcessorMode) =
      ProcessorState(mode)
}