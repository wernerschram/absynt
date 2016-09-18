package assembler.x86.instructions.arithmetic

import assembler.x86.ProcessorMode
import assembler.x86.opcodes.Static
import assembler.x86.operands.FixedSizeModRMEncodableOperand

object Not {
  implicit val opcode = "not"

  val RM8 = new Static(0xF6.toByte :: Nil).withModRM(0x02.toByte)
  val RM16 = new Static(0xF7.toByte :: Nil).withModRM(0x02.toByte)

  def apply(operand: FixedSizeModRMEncodableOperand)(implicit processorMode: ProcessorMode) = operand.operandByteSize match {
    case 1 => RM8(operand)
    case _ => RM16(operand)
  }
}