package assembler.x86.instructions

import assembler.x86.ProcessorMode
import assembler.x86.opcodes.Static
import assembler.x86.operands.ImmediateValue

object Interrupt {
  implicit val opcode = "int"

  private val Static = new Static(0xCC.toByte :: Nil)
  private val Imm8 = new Static(0xCD.toByte :: Nil).withImmediate({ case (value, _) => value.operandByteSize == 1 })

  def apply()(implicit processorMode: ProcessorMode) = 
    Static()
  
  def apply(immediate: ImmediateValue)(implicit processorMode: ProcessorMode) = immediate.value.head match {
    case 3 => Static()
    case _ => Imm8(immediate)
  }

}