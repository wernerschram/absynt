package assembler.x86.operations

import assembler.x86.operands.ImmediateValue
import assembler.x86.operations.OperandInfo.OperandOrder._

trait Immediate extends X86Operation with ImmediateBytes {

  // TODO: remove extends X86Operation so that self type can be restricted
  self: X86Operation with ModRMBytes with DisplacementBytes =>
  def immediate: ImmediateValue
  def immediateOrder: OperandOrder

  abstract override def operands: Set[OperandInfo] = super.operands + OperandInfo.immediate(immediate, immediateOrder)

  override def immediateBytes: Seq[Byte] = immediate.value
}
