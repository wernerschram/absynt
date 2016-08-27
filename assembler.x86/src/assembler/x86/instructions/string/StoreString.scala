package assembler.x86.instructions.string

import assembler.x86.ProcessorMode
import assembler.x86.opcodes.Static
import assembler.x86.operands.memoryaccess.RegisterMemoryLocation
import assembler.x86.operands.registers.AccumulatorRegister
import assembler.x86.operands.registers.DestinationIndex
import assembler.x86.operands.registers.Register

final object StoreString {
  implicit val mnemonic = "stos"

  val validate: PartialFunction[(AccumulatorRegister, RegisterMemoryLocation, ProcessorMode), Boolean] = { case _ => true }

  private val Static8 = (new Static(0xAA.toByte :: Nil)).asTwoOperandOpcode[AccumulatorRegister, RegisterMemoryLocation](validate)
  private val Static16 = (new Static(0xAB.toByte :: Nil)).asTwoOperandOpcode[AccumulatorRegister, RegisterMemoryLocation](validate)

  private val RepStatic8 = (new Static(0xAA.toByte :: Nil)).asTwoOperandOpcode[AccumulatorRegister, RegisterMemoryLocation](validate).repeated()
  private val RepStatic16 = (new Static(0xAB.toByte :: Nil)).asTwoOperandOpcode[AccumulatorRegister, RegisterMemoryLocation](validate).repeated()

  def apply(register: AccumulatorRegister, destination: DestinationIndex)(implicit processorMode: ProcessorMode) = (register, destination) match {
    case (Register.AL, _) => Static8(register, RegisterMemoryLocation(destination))
    case _ => Static16(register, RegisterMemoryLocation(destination))
  }

  object Repeat {
    def apply(register: AccumulatorRegister, destination: RegisterMemoryLocation.DIref)(implicit processorMode: ProcessorMode) = (register, destination) match {
      case (Register.AL, _) => RepStatic8(register, destination)
      case _ => RepStatic16(register, destination)
    }
  }
}