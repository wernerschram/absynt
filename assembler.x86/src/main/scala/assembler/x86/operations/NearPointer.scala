package assembler.x86.operations

import assembler.x86.operands.memoryaccess.{NearPointer => NearPointerType}
import assembler.x86.operations.OperandInfo.OperandOrder.OperandOrder

trait NearPointer extends X86Operation with DisplacementBytes {

  // TODO: remove extends X86Operation so that self type can be restricted
  self: X86Operation with ModRMBytes with DisplacementBytes with ImmediateBytes =>
  def pointer: NearPointerType

  def pointerOrder: OperandOrder

  override def displacementBytes: Seq[Byte] = pointer.encodeBytes

  abstract override def operands: Set[OperandInfo] = super.operands + OperandInfo.relative(pointer, pointerOrder)
}
