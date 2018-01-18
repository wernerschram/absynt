package assembler.x86.instructions

import assembler.x86.ProcessorMode
import assembler.x86.operands.{OperandSize, ValueSize}
import assembler.x86.operations.Static

object SystemCall {
  val opcode = "syscall"


  def apply()(implicit processorMode: ProcessorMode): Static = {
    assume(processorMode == ProcessorMode.Long)
    Static()
  }

  private def Static()(implicit processorMode: ProcessorMode) = new Static(0x0F.toByte :: 0x05.toByte :: Nil, opcode)

}

object SystemEnter {
  val opcode = "sysenter"


  def apply()(implicit processorMode: ProcessorMode): Static = {
    assume(processorMode != ProcessorMode.Real)
    Static()
  }

  private def Static()(implicit processorMode: ProcessorMode) = new Static(0x0F.toByte :: 0x34.toByte :: Nil, opcode)

}

object SystemReturn {
  val opcode = "sysret"


  def apply(returnMode: ProcessorMode)(implicit processorMode: ProcessorMode): Static = {
    assume(processorMode == ProcessorMode.Long && returnMode != ProcessorMode.Real)
    Static(returnMode)
  }

  private def Static(returnMode: ProcessorMode)(implicit processorMode: ProcessorMode) =
    new Static(0x0F.toByte :: 0x07.toByte :: Nil, opcode) {
       override def operandSize: OperandSize = returnMode match {
        case ProcessorMode.Long => ValueSize.QuadWord
        case ProcessorMode.Protected => ValueSize.DoubleWord
        case ProcessorMode.Real => throw new AssertionError
      }
    }

}

object SystemExit {
  val opcode = "sysexit"


  def apply(returnMode: ProcessorMode)(implicit processorMode: ProcessorMode): Static = {
    assume(processorMode != ProcessorMode.Real && returnMode != ProcessorMode.Real)
    assume(processorMode == ProcessorMode.Protected && returnMode == ProcessorMode.Protected || processorMode == ProcessorMode.Long)
    Static(returnMode)
  }

  private def Static(returnMode: ProcessorMode)(implicit processorMode: ProcessorMode) =
    new Static(0x0F.toByte :: 0x35.toByte :: Nil, opcode) {
      override def operandSize: OperandSize = returnMode match {
        case ProcessorMode.Long => ValueSize.QuadWord
        case ProcessorMode.Protected => ValueSize.DoubleWord
        case ProcessorMode.Real => throw new AssertionError
      }
    }

}
