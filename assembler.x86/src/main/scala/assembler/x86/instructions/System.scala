package assembler.x86.instructions

import assembler.Label
import assembler.x86.ProcessorMode
import assembler.x86.operations.Static

object SystemCall {
  val opcode = "syscall"


  def apply()(implicit label: Label, processorMode: ProcessorMode): Static = {
    assume(processorMode == ProcessorMode.Long)
    Static()
  }

  private def Static()(implicit label: Label, processorMode: ProcessorMode) = new Static(label, 0x0F.toByte :: 0x05.toByte :: Nil, opcode)

}

object SystemEnter {
  val opcode = "sysenter"


  def apply()(implicit label: Label, processorMode: ProcessorMode): Static = {
    assume(processorMode != ProcessorMode.Real)
    Static()
  }

  private def Static()(implicit label: Label, processorMode: ProcessorMode) = new Static(label, 0x0F.toByte :: 0x34.toByte :: Nil, opcode)

}

object SystemReturn {
  val opcode = "sysret"


  def apply()(implicit label: Label, processorMode: ProcessorMode): Static = {
    assume(processorMode == ProcessorMode.Long)
    assume(processorMode != ProcessorMode.Real)
    Static()
  }

  private def Static()(implicit label: Label, processorMode: ProcessorMode) = new Static(label, 0x0F.toByte :: 0x07.toByte :: Nil, opcode)

}

object SystemExit {
  val opcode = "sysexit"


  def apply()(implicit label: Label, processorMode: ProcessorMode): Static = {
    assume(processorMode != ProcessorMode.Real)
    Static()
  }

  private def Static()(implicit label: Label, processorMode: ProcessorMode) = new Static(label, 0x0F.toByte :: 0x35.toByte :: Nil, opcode)

}
