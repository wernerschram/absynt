package assembler.x86.instructions

import assembler.x86.ProcessorMode
import assembler.x86.operands.ValueSize.Byte
import assembler.x86.operands._
import assembler.x86.operands.memoryaccess.RegisterMemoryLocation
import assembler.x86.operations.{Repeated, ReversedOperands, Static}

object StoreString {
  implicit val mnemonic = "stos"

  def apply(register: AccumulatorRegister, destination: RegisterMemoryLocation.DIReference)
           (implicit processorMode: ProcessorMode): Static with ReversedOperands =
    (register, destination) match {
      case (Register.AL, _) => Static8(destination)
      case _ => Static16(register, destination)
    }

  private def Static8(destination: RegisterMemoryLocation.DIReference)(implicit processorMode: ProcessorMode) =
    new Static(0xAA.toByte :: Nil, mnemonic) with ReversedOperands {
      override def operands: List[ModRMEncodableOperand] = destination :: Register.AL :: Nil

      override def operandSize: Byte.type = Register.AL.operandByteSize

      override def addressSize: OperandSize = destination.addressSize
    }

  private def Static16(register: AccumulatorRegister, destination: RegisterMemoryLocation.DIReference)
                      (implicit processorMode: ProcessorMode) =
    new Static(0xAB.toByte :: Nil, mnemonic) with ReversedOperands {
      override def operands: List[ModRMEncodableOperand] = destination :: register :: Nil

      override def operandSize: OperandSize = register.operandByteSize

      override def addressSize: OperandSize = destination.addressSize
    }

  private def RepStatic8(destination: RegisterMemoryLocation.DIReference)(implicit processorMode: ProcessorMode) =
    new Static(0xAA.toByte :: Nil, mnemonic) with Repeated with ReversedOperands {
      override def operands: List[ModRMEncodableOperand] = destination :: Register.AL :: Nil

      override def operandSize: Byte.type = Register.AL.operandByteSize

      override def addressSize: OperandSize = destination.addressSize
    }

  private def RepStatic16(register: AccumulatorRegister, destination: RegisterMemoryLocation.DIReference)
                         (implicit processorMode: ProcessorMode) =
    new Static(0xAB.toByte :: Nil, mnemonic) with Repeated with ReversedOperands {
      override def operands: List[ModRMEncodableOperand] = destination :: register :: Nil

      override def operandSize: OperandSize = register.operandByteSize

      override def addressSize: OperandSize = destination.addressSize
    }

  object Repeat {
    def apply(register: AccumulatorRegister, destination: RegisterMemoryLocation.DIReference)
             (implicit processorMode: ProcessorMode): Static with Repeated with ReversedOperands =
      (register, destination) match {
        case (Register.AL, _) => RepStatic8(destination)
        case _ => RepStatic16(register, destination)
      }
  }

}