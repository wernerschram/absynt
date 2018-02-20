package assembler.x86.instructions

import assembler.x86.ProcessorMode
import assembler.x86.operands.{ByteSize, ImmediateValue, ValueSize}
import assembler.x86.operations.{Immediate, OperandInfo, Static}
import assembler.x86.operations.OperandInfo.OperandOrder._

object Interrupt {
  implicit val opcode: String = "int"

  def apply(immediate: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode): Static = immediate.value.head match {
    case 3 => Static()
    case _ => Imm8(immediate)
  }

  private def Static()(implicit processorMode: ProcessorMode) = new Static(0xCC.toByte :: Nil, opcode) {
    override def operands: Set[OperandInfo] = super.operands + OperandInfo.implicitOperand(ImmediateValue(3.toByte), destination)
  }

  private def Imm8(immediateValue: ImmediateValue with ByteSize)(implicit processorMode: ProcessorMode) =
    new Static(0xCD.toByte :: Nil, opcode) with Immediate {
      override def immediate: ImmediateValue = immediateValue

      override def immediateOrder: OperandOrder = destination
    }

}