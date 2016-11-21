package assembler.x86.instructions

import assembler.x86.ProcessorMode
import assembler.x86.operands.ImmediateValue
import assembler.x86.operations.Immediate
import assembler.x86.operations.Static

object Interrupt {
  implicit val opcode = "int"


  private def Static()(implicit processorMode: ProcessorMode) = new Static(0xCC.toByte :: Nil, opcode)
  private def Imm8(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) = new Static(0xCD.toByte :: Nil, opcode) with Immediate {
    override def immediate = immediateValue
    override def validate = {
      super.validate
      assume(immediate.operandByteSize == 1)
    }
  }

  def apply()(implicit processorMode: ProcessorMode) =
    Static()

  def apply(immediate: ImmediateValue)(implicit processorMode: ProcessorMode) = immediate.value.head match {
    case 3 => Static()
    case _ => Imm8(immediate)
  }

}