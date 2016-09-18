package assembler.x86.instructions

import assembler.x86.ProcessorMode
import assembler.x86.opcodes.Static

object SetInterruptFlag {
  implicit val mnemonic = "sti"
  
  private val Static = new Static(0xFB.toByte :: Nil)

  def apply()(implicit processorMode: ProcessorMode) = 
    Static()
}