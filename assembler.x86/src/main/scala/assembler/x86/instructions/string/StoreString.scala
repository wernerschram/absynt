package assembler.x86.instructions.string

import assembler.x86.ProcessorMode
import assembler.x86.operands.memoryaccess.RegisterMemoryLocation
import assembler.x86.operands._
import assembler.x86.operations.Static
import assembler.x86.operations.Repeated
import assembler.x86.operations.ReversedOperands

final object StoreString {
  implicit val mnemonic = "stos"

  private def Static8(destination: RegisterMemoryLocation.DIReference)(implicit processorMode: ProcessorMode) =
    new Static(0xAA.toByte :: Nil, mnemonic) with ReversedOperands {
      override def operands = destination :: Register.AL :: Nil
      override def operandSize = Register.AL.operandByteSize
      override def addressSize = destination.addressSize
    }
  private def Static16(register: AccumulatorRegister, destination: RegisterMemoryLocation.DIReference)(implicit processorMode: ProcessorMode) =
    new Static(0xAB.toByte :: Nil, mnemonic) with ReversedOperands {
      override def operands = destination :: register :: Nil
      override def operandSize = register.operandByteSize
      override def addressSize = destination.addressSize
    }

  private def RepStatic8(destination: RegisterMemoryLocation.DIReference)(implicit processorMode: ProcessorMode) =
    new Static(0xAA.toByte :: Nil, mnemonic) with Repeated with ReversedOperands {
      override def operands = destination :: Register.AL :: Nil
      override def operandSize = Register.AL.operandByteSize
      override def addressSize = destination.addressSize
    }
  private def RepStatic16(register: AccumulatorRegister, destination: RegisterMemoryLocation.DIReference)(implicit processorMode: ProcessorMode) =
    new Static(0xAB.toByte :: Nil, mnemonic) with Repeated with ReversedOperands {
      override def operands = destination :: register :: Nil
      override def operandSize = register.operandByteSize
      override def addressSize = destination.addressSize
    }

  def apply(register: AccumulatorRegister, destination: RegisterMemoryLocation.DIReference)(implicit processorMode: ProcessorMode) = (register, destination) match {
    case (Register.AL, _) => Static8(destination)
    case _ => Static16(register, destination)
  }

  object Repeat {
    def apply(register: AccumulatorRegister, destination: RegisterMemoryLocation.DIReference)(implicit processorMode: ProcessorMode) = (register, destination) match {
      case (Register.AL, _) => RepStatic8(destination)
      case _ => RepStatic16(register, destination)
    }
  }
}