package assembler.x86.instructions

import assembler.Label
import assembler.x86.ProcessorMode
import assembler.x86.operands.{ImmediateValue, Operand, ValueSize}
import assembler.x86.operations.{Immediate, Static}

object Interrupt {
  implicit val opcode = "int"

  def apply(immediate: ImmediateValue)(implicit processorMode: ProcessorMode, label: Label): Static = immediate.value.head match {
    case 3 => Static()
    case _ => Imm8(immediate)
  }

  private def Static()(implicit processorMode: ProcessorMode, label: Label) = new Static(label, 0xCC.toByte :: Nil, opcode) {
    override def operands: List[Operand] = ImmediateValue.byteToImmediate(3.toByte) :: super.operands
  }

  private def Imm8(immediateValue: ImmediateValue)(implicit processorMode: ProcessorMode, label: Label) =
    new Static(label, 0xCD.toByte :: Nil, opcode) with Immediate {
      override def immediate: ImmediateValue = immediateValue

      override def validate(): Unit = {
        super.validate()
        assume(immediate.operandByteSize == ValueSize.Byte)
      }
    }

}