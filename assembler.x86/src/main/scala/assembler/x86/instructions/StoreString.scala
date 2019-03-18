package assembler.x86.instructions

import assembler.x86.{HasOperandSizePrefixRequirements, ProcessorMode}
import assembler.x86.operands._
import assembler.x86.operands.memoryaccess.DestinationReference
import assembler.x86.operations.OperandInfo.OperandOrder
import assembler.x86.operations._

object StoreString {
  implicit val mnemonic: String = "stos"

  trait Common {
    self: HasOperandSizePrefixRequirements =>
    protected def Static8(destination: DestinationReference with ByteSize)(implicit processorMode: ProcessorMode, operandSizePrefixRequirement: OperandSizePrefixRequirement): X86Operation =
      new Static(0xAA.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
        override protected def implicitInit(): Unit = {
          addOperand(OperandInfo.implicitAddress(destination, OperandOrder.destination))
          addOperand(OperandInfo.implicitOperand(Accumulator.LowByte, OperandOrder.source))
        }
      }

    protected def Static16[Size <: WordDoubleQuadSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size)
                                                    (implicit processorMode: ProcessorMode, operandSizePrefixRequirement: OperandSizePrefixRequirement): X86Operation =
      new Static(0xAB.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
        override protected def implicitInit(): Unit = {
          addOperand(OperandInfo.implicitAddress(destination, OperandOrder.destination))
          addOperand(OperandInfo.implicitOperand(register, OperandOrder.source))
        }
      }

    protected def RepStatic8(destination: DestinationReference with ByteSize)(implicit processorMode: ProcessorMode, operandSizePrefixRequirement: OperandSizePrefixRequirement): X86Operation =
      new Static(0xAA.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate with Repeated {
        override protected def implicitInit(): Unit = {
          addOperand(OperandInfo.implicitAddress(destination, OperandOrder.destination))
          addOperand(OperandInfo.implicitOperand(Accumulator.LowByte, OperandOrder.source))
        }
      }

    protected def RepStatic16[Size <: WordDoubleQuadSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size)
                           (implicit processorMode: ProcessorMode, operandSizePrefixRequirement: OperandSizePrefixRequirement): X86Operation =
      new Static(0xAB.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate with Repeated {
        override protected def implicitInit(): Unit = {
          addOperand(OperandInfo.implicitAddress(destination, OperandOrder.destination))
          addOperand(OperandInfo.implicitOperand(register, OperandOrder.source))
        }
      }
  }

  trait LegacyOperations extends Common {
    self: HasOperandSizePrefixRequirements =>
    object StoreString {
      def apply[Size <: ByteWordSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size): X86Operation =
        (register, destination) match {
          case (Accumulator.LowByte, d: DestinationReference with ByteSize) => Static8(d)
          case (a: AccumulatorRegister with WordSize, d: DestinationReference with WordSize) => Static16(a, d)
        }

      object Repeat {
        def apply[Size <: ByteWordSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size): X86Operation =
          (register, destination) match {
            case (Accumulator.LowByte, d: DestinationReference with ByteSize) => RepStatic8(d)
            case (a: AccumulatorRegister with WordSize, d: DestinationReference with WordSize) => RepStatic16(a, d)
          }
      }
    }
  }

  trait RealOperations extends Common {
    self: HasOperandSizePrefixRequirements =>
    object StoreString {
      def apply[Size <: ByteWordDoubleSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size): X86Operation =
        (register, destination) match {
          case (Accumulator.LowByte, d: DestinationReference with ByteSize) => Static8(d)
          case (a: AccumulatorRegister with WordDoubleSize, d: DestinationReference with WordDoubleSize) => Static16(a, d)
        }

      object Repeat {
        def apply[Size <: ByteWordDoubleSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size): X86Operation =
          (register, destination) match {
            case (Accumulator.LowByte, d: DestinationReference with ByteSize) => RepStatic8(d)
            case (a: AccumulatorRegister with WordSize, d: DestinationReference with WordSize) => RepStatic16(a, d)
          }
      }
    }
  }

  trait ProtectedOperations extends Common {
    self: HasOperandSizePrefixRequirements =>
    object StoreString {
      def apply[Size <: ByteWordDoubleSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size): X86Operation =
        (register, destination) match {
          case (Accumulator.LowByte, d: DestinationReference with ByteSize) => Static8(d)
          case (a: AccumulatorRegister with WordDoubleSize, d: DestinationReference with WordDoubleSize) => Static16(a, d)
        }

      object Repeat {
        def apply[Size <: ByteWordDoubleSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size): X86Operation =
          (register, destination) match {
            case (Accumulator.LowByte, d: DestinationReference with ByteSize) => RepStatic8(d)
            case (a: AccumulatorRegister with WordSize, d: DestinationReference with WordSize) => RepStatic16(a, d)
          }
      }
    }
  }

  trait LongOperations extends Common {
    self: HasOperandSizePrefixRequirements =>
    object StoreString {
      def apply[Size <: ValueSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size): X86Operation =
        (register, destination) match {
          case (Accumulator.LowByte, d: DestinationReference with ByteSize) => Static8(d)
          case (a: AccumulatorRegister with WordDoubleQuadSize, d: DestinationReference with WordDoubleQuadSize) => Static16(a, d)
        }

      object Repeat {
        def apply[Size <: ValueSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size): X86Operation =
          (register, destination) match {
            case (Accumulator.LowByte, d: DestinationReference with ByteSize) => RepStatic8(d)
            case (a: AccumulatorRegister with WordSize, d: DestinationReference with WordSize) => RepStatic16(a, d)
          }
      }
    }
  }
}