package assembler.x86.instructions

import assembler.x86.ProcessorMode
import assembler.x86.operands.Register.I8086Registers
import assembler.x86.operands._
import assembler.x86.operands.memoryaccess.DestinationReference
import assembler.x86.operations.OperandInfo.OperandOrder
import assembler.x86.operations._

object StoreString extends I8086Registers {
  implicit val mnemonic: String = "stos"

  def apply[Size<:ValueSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size)
           (implicit processorMode: ProcessorMode): Static =
    (register, destination) match {
      case (AL, d: DestinationReference with ByteSize) => Static8(d)
      case (_, d: DestinationReference with WideSize) => Static16(register, d)
    }

  private def Static8(destination: DestinationReference with ByteSize)(implicit processorMode: ProcessorMode) =
    new Static(0xAA.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
      override protected def implicitInit(): Unit = {
        addOperand(OperandInfo.implicitAddress(destination, OperandOrder.destination))
        addOperand(OperandInfo.implicitOperand(AL, OperandOrder.source))
      }
    }

  private def Static16[Size<:WideSize](register: AccumulatorRegister, destination: DestinationReference with Size)
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
        addOperand(OperandInfo.implicitOperand(AL, OperandOrder.source))
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

  object Repeat {
    def apply[Size<:ValueSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size)
             (implicit processorMode: ProcessorMode): Static with Repeated =
      (register, destination) match {
        case (AL, _) => RepStatic8(destination)
        case _ => RepStatic16(register, destination)
      }
  }

}