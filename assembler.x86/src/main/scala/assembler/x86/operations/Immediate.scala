package assembler.x86.operations

import assembler.x86.operands.ImmediateValue
import assembler.x86.operations.OperandInfo.OperandOrder._

trait Immediate extends ImmediateBytes {

  self: X86Operation =>
  def immediate: ImmediateValue
  def immediateOrder: OperandOrder

  override protected def immediateInit(): Unit =
    addOperand(OperandInfo.immediate(immediate, immediateOrder))

  override def immediateBytes: Seq[Byte] = immediate.value
}
