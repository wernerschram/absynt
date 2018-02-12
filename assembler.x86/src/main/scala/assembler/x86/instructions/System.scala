package assembler.x86.instructions

import assembler.x86.ProcessorMode
import assembler.x86.operands.{OperandSize, ReturnMode, ValueSize}
import assembler.x86.operations.{OperandInfo, Static}
import assembler.x86.operations.OperandInfo.OperandOrder._

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


  def apply(returnMode: ReturnMode)(implicit processorMode: ProcessorMode): Static = {
    assume(processorMode == ProcessorMode.Long)
    Static(returnMode)
  }

  private def Static(returnMode: ReturnMode)(implicit processorMode: ProcessorMode) =
    new Static(0x0F.toByte :: 0x07.toByte :: Nil, opcode) {
      override val operands: Seq[OperandInfo] = Seq(OperandInfo.implicitOperand(returnMode, first))
    }
}

object SystemExit {
  val opcode = "sysexit"


  def apply(returnMode: ReturnMode)(implicit processorMode: ProcessorMode): Static = {
    assume(processorMode == ProcessorMode.Protected && returnMode == ReturnMode.Protected || processorMode == ProcessorMode.Long)
    Static(returnMode)
  }

  private def Static(returnMode: ReturnMode)(implicit processorMode: ProcessorMode) =
    new Static(0x0F.toByte :: 0x35.toByte :: Nil, opcode) {
      override val operands: Seq[OperandInfo] = Seq(OperandInfo.implicitOperand(returnMode, first))
    }
}
