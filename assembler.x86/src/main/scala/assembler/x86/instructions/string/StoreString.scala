package assembler.x86.instructions.string

import assembler.x86.ProcessorMode
import assembler.x86.operands.memoryaccess.RegisterMemoryLocation
import assembler.x86.operands._
import assembler.x86.operations.Static
import assembler.x86.operations.Repeated

final object StoreString {
  implicit val mnemonic = "stos"

  val validate: PartialFunction[(AccumulatorRegister, RegisterMemoryLocation, ProcessorMode), Boolean] = { case _ => true }


  private def Static8(destination: RegisterMemoryLocation.DIref)(implicit processorMode: ProcessorMode) = new Static(0xAA.toByte :: Nil, mnemonic) {
    override def operands = destination :: Register.AL :: Nil
    override def operandSize = Some(Register.AL.operandByteSize)
    override def addressSize = Some(destination.addressSize)
  }
  private def Static16(register: AccumulatorRegister, destination: RegisterMemoryLocation.DIref)(implicit processorMode: ProcessorMode) = new Static(0xAB.toByte :: Nil, mnemonic) {
    override def operands = destination :: register :: Nil
    override def operandSize = Some(register.operandByteSize)
    override def addressSize = Some(destination.addressSize)
  }

  private def RepStatic8(destination: RegisterMemoryLocation.DIref)(implicit processorMode: ProcessorMode) = new Static(0xAA.toByte :: Nil, mnemonic) with Repeated {
    override def operands = destination :: Register.AL :: Nil
    override def operandSize = Some(Register.AL.operandByteSize)
    override def addressSize = Some(destination.addressSize)
  }
  private def RepStatic16(register: AccumulatorRegister, destination: RegisterMemoryLocation.DIref)(implicit processorMode: ProcessorMode) = new Static(0xAB.toByte :: Nil, mnemonic) with Repeated {
    override def operands = destination :: register :: Nil
    override def operandSize = Some(register.operandByteSize)
    override def addressSize = Some(destination.addressSize)
  }

  def apply(register: AccumulatorRegister, destination: RegisterMemoryLocation.DIref)(implicit processorMode: ProcessorMode) = (register, destination) match {
    case (Register.AL, _) => Static8(destination)
    case _ => Static16(register, destination)
  }

  object Repeat {
    def apply(register: AccumulatorRegister, destination: RegisterMemoryLocation.DIref)(implicit processorMode: ProcessorMode) = (register, destination) match {
      case (Register.AL, _) => RepStatic8(destination)
      case _ => RepStatic16(register, destination)
    }
  }
}