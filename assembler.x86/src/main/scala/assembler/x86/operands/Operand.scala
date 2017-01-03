package assembler.x86.operands

import assembler.x86.ProcessorMode

trait Operand {
  def isValidForMode(processorMode: ProcessorMode): Boolean = true // linter:ignore UnusedParameter
}