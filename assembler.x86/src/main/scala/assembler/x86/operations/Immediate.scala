package assembler.x86.operations

import assembler.x86.operands.{ImmediateValue, OperandSize}
import assembler.x86.operations.OperandInfo.OperandOrder._

trait Immediate extends X86Operation {

  self: X86Operation =>
  def immediate: ImmediateValue
  def immediateOrder: OperandOrder

  abstract override def operands: Seq[OperandInfo] = super.operands :+ OperandInfo.immediate(immediate, immediateOrder)

  abstract override def encodeByte: Seq[Byte] =
    super.encodeByte ++ immediate.value
}
