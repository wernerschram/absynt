package assembler.x86.instructions

import assembler.x86.{HasOperandSizePrefixRequirements, ProcessorMode}
import assembler.x86.operands.ReturnMode
import assembler.x86.operations.OperandInfo.OperandOrder._
import assembler.x86.operations._

object System {
  trait Common {
    self: HasOperandSizePrefixRequirements =>


    def staticEnter(): Static =
      new Static(0x0F.toByte :: 0x34.toByte :: Nil, "sysenter") with NoDisplacement with NoImmediate


    def staticExit(returnMode: ReturnMode): Static =
      new Static(0x0F.toByte :: 0x35.toByte :: Nil, "sysexit") with NoDisplacement with NoImmediate {
        override protected def implicitInit(): Unit =
          addOperand(OperandInfo.implicitOperand(returnMode, destination))
      }
  }

  trait ProtectedOperations extends Common {
    self: HasOperandSizePrefixRequirements =>
    object SystemEnter {
      def apply()(implicit processorMode: ProcessorMode): Static = staticEnter()
    }

    object SystemExit {
      def apply(returnMode: ReturnMode)(implicit processorMode: ProcessorMode): Static = staticExit(returnMode)
    }
  }

  trait LongOperations extends Common {
    self: HasOperandSizePrefixRequirements =>

    def staticCall(): Static =
      new Static(0x0F.toByte :: 0x05.toByte :: Nil, "syscall") with NoDisplacement with NoImmediate

    def staticReturn(returnMode: ReturnMode): Static =
      new Static(0x0F.toByte :: 0x07.toByte :: Nil, "sysret") with NoDisplacement with NoImmediate {
        override protected def implicitInit(): Unit =
          addOperand(OperandInfo.implicitOperand(returnMode, destination))
      }

    object SystemCall {
      def apply(): Static = staticCall()
    }

    object SystemEnter {
      def apply()(implicit processorMode: ProcessorMode): Static = staticEnter()
    }

    object SystemReturn {
      def apply(returnMode: ReturnMode): Static = staticReturn(returnMode)
    }

    object SystemExit {
      def apply(returnMode: ReturnMode)(implicit processorMode: ProcessorMode): Static = staticExit(returnMode)
    }

  }
}
