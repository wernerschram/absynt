package assembler.x86.instructions

import assembler.x86.ProcessorMode
import assembler.x86.operands._
import assembler.x86.operands.memoryaccess.DestinationReference
import assembler.x86.operations.OperandInfo.OperandOrder
import assembler.x86.operations._

object StoreString {
  implicit val mnemonic: String = "stos"

  private def Static8(destination: DestinationReference with ByteSize)(implicit processorMode: ProcessorMode) =
    new Static(0xAA.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
      override protected def implicitInit(): Unit = {
        addOperand(OperandInfo.implicitAddress(destination, OperandOrder.destination))
        addOperand(OperandInfo.implicitOperand(Accumulator.LowByte, OperandOrder.source))
      }
    }

  private def Static16[Size<:WordDoubleQuadSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size)
                                                (implicit processorMode: ProcessorMode) =
    new Static(0xAB.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
      override protected def implicitInit(): Unit = {
        addOperand(OperandInfo.implicitAddress(destination, OperandOrder.destination))
        addOperand(OperandInfo.implicitOperand(register, OperandOrder.source))
      }
    }

  private def RepStatic8(destination: DestinationReference)(implicit processorMode: ProcessorMode) =
    new Static(0xAA.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate with Repeated {
      override protected def implicitInit(): Unit = {
        addOperand(OperandInfo.implicitAddress(destination, OperandOrder.destination))
        addOperand(OperandInfo.implicitOperand(Accumulator.LowByte, OperandOrder.source))
      }
    }

  private def RepStatic16(register: AccumulatorRegister, destination: DestinationReference)
                         (implicit processorMode: ProcessorMode) =
    new Static(0xAB.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate with Repeated {
      override protected def implicitInit(): Unit = {
        addOperand(OperandInfo.implicitAddress(destination, OperandOrder.destination))
        addOperand(OperandInfo.implicitOperand(register, OperandOrder.source))
      }
    }


  trait LegacyOperations {
    object StoreString {
      def apply[Size <: ByteWordSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size): Static =
        (register, destination) match {
          case (Accumulator.LowByte, d: DestinationReference with ByteSize) => Static8(d)(ProcessorMode.Legacy)
          case (a: AccumulatorRegister with WordSize, d: DestinationReference with WordSize) => Static16(a, d)(ProcessorMode.Legacy)
        }

      object Repeat {
        def apply[Size <: ByteWordSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size): Static with Repeated =
          (register, destination) match {
            case (Accumulator.LowByte, _) => RepStatic8(destination)(ProcessorMode.Legacy)
            case (_, d: DestinationReference with WordSize) => RepStatic16(register, d)(ProcessorMode.Legacy)
          }
      }
    }
  }

  trait RealOperations {
    object StoreString {
      def apply[Size <: ByteWordDoubleSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size): Static =
        (register, destination) match {
          case (Accumulator.LowByte, d: DestinationReference with ByteSize) => Static8(d)(ProcessorMode.Real)
          case (a: AccumulatorRegister with WordDoubleSize, d: DestinationReference with WordDoubleSize) => Static16(a, d)(ProcessorMode.Real)
        }

      object Repeat {
        def apply[Size <: ByteWordDoubleSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size): Static with Repeated =
          (register, destination) match {
            case (Accumulator.LowByte, _) => RepStatic8(destination)(ProcessorMode.Real)
            case (_, d: DestinationReference with WordDoubleSize) => RepStatic16(register, d)(ProcessorMode.Real)
          }
      }
    }
  }

  trait ProtectedOperations {
    object StoreString {
      def apply[Size <: ByteWordDoubleSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size): Static =
        (register, destination) match {
          case (Accumulator.LowByte, d: DestinationReference with ByteSize) => Static8(d)(ProcessorMode.Protected)
          case (a: AccumulatorRegister with WordDoubleSize, d: DestinationReference with WordDoubleSize) => Static16(a, d)(ProcessorMode.Protected)
        }

      object Repeat {
        def apply[Size <: ByteWordDoubleSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size): Static with Repeated =
          (register, destination) match {
            case (Accumulator.LowByte, _) => RepStatic8(destination)(ProcessorMode.Protected)
            case (_, d: DestinationReference with WordDoubleSize) => RepStatic16(register, d)(ProcessorMode.Protected)
          }
      }
    }
  }

  trait LongOperations {
    object StoreString {
      def apply[Size <: ValueSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size): Static =
        (register, destination) match {
          case (Accumulator.LowByte, d: DestinationReference with ByteSize) => Static8(d)(ProcessorMode.Long)
          case (a: AccumulatorRegister with WordDoubleQuadSize, d: DestinationReference with WordDoubleQuadSize) => Static16(a, d)(ProcessorMode.Long)
        }

      object Repeat {
        def apply[Size <: ValueSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size): Static with Repeated =
          (register, destination) match {
            case (Accumulator.LowByte, _) => RepStatic8(destination)(ProcessorMode.Long)
            case (_, d: DestinationReference with WordDoubleQuadSize) => RepStatic16(register, d)(ProcessorMode.Long)
          }
      }
    }
  }
}