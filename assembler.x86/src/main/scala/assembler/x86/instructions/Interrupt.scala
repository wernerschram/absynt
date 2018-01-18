package assembler.x86.instructions

import assembler.x86.ProcessorMode
import assembler.x86.operands.{ImmediateValue, Operand, ValueSize}
import assembler.x86.operations.{Immediate, Static}

object Interrupt {
  implicit val opcode: String = "int"

  def apply(immediate: ImmediateValue)(implicit processorMode: ProcessorMode): Static = immediate.value.head match {
    case 3 => Static()
    case _ => Imm8(immediate)
  }

  private def Static()(implicit processorMode: ProcessorMode) = new Static(0xCC.toByte :: Nil, opcode) {
    override def operands: Seq[Operand] = ImmediateValue.byteToImmediate(3.toByte) +: super.operands
  }

  private def Imm8(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode) =
    new Static(0xCD.toByte :: Nil, opcode) with Immediate {
      override def immediate: ImmediateValue = immediateValue

      override def validate(): Unit = {
        super.validate()
        assume(immediate.operandByteSize == ValueSize.Byte)
      }
    }

}