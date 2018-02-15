package assembler.x86.instructions

import assembler.x86.ProcessorMode
import assembler.x86.operands._
import assembler.x86.operands.memoryaccess.RegisterMemoryLocation
import assembler.x86.operations.OperandInfo.OperandOrder._
import assembler.x86.operations.{OperandInfo, Repeated, Static}

object StoreString {
  implicit val mnemonic: String = "stos"

  def apply(register: AccumulatorRegister, destination: RegisterMemoryLocation[DestinationIndex with IndexRegister])
           (implicit processorMode: ProcessorMode): Static =
    (register, destination) match {
      case (Register.AL, _) => Static8(destination)
      case _ => Static16(register, destination)
    }

  private def Static8(destination: RegisterMemoryLocation[DestinationIndex with IndexRegister])(implicit processorMode: ProcessorMode) =
    new Static(0xAA.toByte :: Nil, mnemonic) {
      override def operands: Seq[OperandInfo] = Seq(OperandInfo.implicitAddress(destination, first), OperandInfo.implicitOperand(Register.AL, second))
    }

  private def Static16(register: AccumulatorRegister, destination: RegisterMemoryLocation[DestinationIndex with IndexRegister])
                      (implicit processorMode: ProcessorMode) =
    new Static(0xAB.toByte :: Nil, mnemonic) {
      override def operands: Seq[OperandInfo] = Seq(OperandInfo.implicitAddress(destination, first), OperandInfo.implicitOperand(register, second))
    }

  private def RepStatic8(destination: RegisterMemoryLocation[DestinationIndex with IndexRegister])(implicit processorMode: ProcessorMode) =
    new Static(0xAA.toByte :: Nil, mnemonic) with Repeated {
      override def operands: Seq[OperandInfo] = Seq(OperandInfo.implicitAddress(destination, first), OperandInfo.implicitOperand(Register.AL, second))
    }

  private def RepStatic16(register: AccumulatorRegister, destination: RegisterMemoryLocation[DestinationIndex with IndexRegister])
                         (implicit processorMode: ProcessorMode) =
    new Static(0xAB.toByte :: Nil, mnemonic) with Repeated {
      override def operands: Seq[OperandInfo] = Seq(OperandInfo.implicitAddress(destination, first), OperandInfo.implicitOperand(register, second))
    }

  object Repeat {
    def apply(register: AccumulatorRegister, destination: RegisterMemoryLocation[DestinationIndex with IndexRegister])
             (implicit processorMode: ProcessorMode): Static with Repeated =
      (register, destination) match {
        case (Register.AL, _) => RepStatic8(destination)
        case _ => RepStatic16(register, destination)
      }
  }

}