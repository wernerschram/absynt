package assembler.x86.instructions

import assembler.x86.ProcessorMode
import assembler.x86.operands._
import assembler.x86.operands.memoryaccess.RegisterMemoryLocation
import assembler.x86.operations.{OperandInfo, Repeated, ReversedOperands, Static}
import assembler.x86.operations.OperandInfo.OperandOrder._

object StoreString {
  implicit val mnemonic: String = "stos"

  // TODO: implement STOS m8 and STOS m16

  def apply(register: AccumulatorRegister, destination: RegisterMemoryLocation[DestinationIndex with IndexRegister])
           (implicit processorMode: ProcessorMode): Static with ReversedOperands =
    (register, destination) match {
      case (Register.AL, _) => Static8(destination)
      case _ => Static16(register, destination)
    }

  private def Static8(destination: RegisterMemoryLocation[DestinationIndex with IndexRegister])(implicit processorMode: ProcessorMode) =
    new Static(0xAA.toByte :: Nil, mnemonic) with ReversedOperands {
      override def operands: Seq[OperandInfo] = Seq(OperandInfo.implicitOperand(destination, first), OperandInfo.implicitOperand(Register.AL, second))

      override def addressSize: OperandSize = destination.addressSize
    }

  private def Static16(register: AccumulatorRegister, destination: RegisterMemoryLocation[DestinationIndex with IndexRegister])
                      (implicit processorMode: ProcessorMode) =
    new Static(0xAB.toByte :: Nil, mnemonic) with ReversedOperands {
      override def operands: Seq[OperandInfo] = Seq(OperandInfo.implicitOperand(destination, first), OperandInfo.implicitOperand(register, second))

      override def addressSize: OperandSize = destination.addressSize
    }

  private def RepStatic8(destination: RegisterMemoryLocation[DestinationIndex with IndexRegister])(implicit processorMode: ProcessorMode) =
    new Static(0xAA.toByte :: Nil, mnemonic) with Repeated with ReversedOperands {
      override def operands: Seq[OperandInfo] = Seq(OperandInfo.implicitOperand(destination, first), OperandInfo.implicitOperand(Register.AL, second))

      override def addressSize: OperandSize = destination.addressSize
    }

  private def RepStatic16(register: AccumulatorRegister, destination: RegisterMemoryLocation[DestinationIndex with IndexRegister])
                         (implicit processorMode: ProcessorMode) =
    new Static(0xAB.toByte :: Nil, mnemonic) with Repeated with ReversedOperands {
      override def operands: Seq[OperandInfo] = Seq(OperandInfo.implicitOperand(destination, first), OperandInfo.implicitOperand(register, second))

      override def addressSize: OperandSize = destination.addressSize
    }

  object Repeat {
    def apply(register: AccumulatorRegister, destination: RegisterMemoryLocation[DestinationIndex with IndexRegister])
             (implicit processorMode: ProcessorMode): Static with Repeated with ReversedOperands =
      (register, destination) match {
        case (Register.AL, _) => RepStatic8(destination)
        case _ => RepStatic16(register, destination)
      }
  }

}