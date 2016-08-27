package assembler.x86.instructions

import assembler.x86.ProcessorMode
import assembler.x86.opcodes.Static
import assembler.x86.operands.ImmediateValue
import assembler.x86.operands.Operand

//class ClearInterruptFlag extends X86Instruction {
//      
//  override def encode()(implicit processorMode: ProcessorMode) : List[Byte] = {
//    assume(isValid())
//    return ClearInterruptFlag()
//  }
//}

object ClearInterruptFlag {
  implicit val opcode = "cli"
  
  private val Static = new Static(0xFA.toByte :: Nil)

  def apply()(implicit processorMode: ProcessorMode) = 
    Static()
}