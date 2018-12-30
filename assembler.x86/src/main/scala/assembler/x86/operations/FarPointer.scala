package assembler.x86.operations

import assembler.x86.operands.memoryaccess.{FarPointer => FarPointerType}
import assembler.x86.operations.OperandInfo.OperandOrder._

trait FarPointer extends X86Operation with DisplacementBytes {

  // TODO: remove extends X86Operation so that self type can be restricted
  self: X86Operation with ModRMBytes with DisplacementBytes with ImmediateBytes =>
  def pointer: FarPointerType

  abstract override def operands: Set[OperandInfo] = super.operands + OperandInfo.pointer(pointer, destination)

  override def displacementBytes: Seq[Byte] = pointer.encodeByte
}
