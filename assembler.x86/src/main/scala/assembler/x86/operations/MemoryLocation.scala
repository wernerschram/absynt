package assembler.x86.operations

import assembler.x86.operands.memoryaccess.{MemoryLocation => MemoryLocationType}
import assembler.x86.operations.OperandInfo.OperandOrder._

trait MemoryLocation extends X86Operation with DisplacementBytes {

  // TODO: remove extends X86Operation so that self type can be restricted
  self: X86Operation with ModRMBytes with ImmediateBytes =>
  def location: MemoryLocationType
  def offsetOrder: OperandOrder

  override protected def displacementInit(): Unit =
    addOperand(OperandInfo.memoryOffset(location, offsetOrder))

  override def displacementBytes: Seq[Byte] = location.displacement.toSeq.flatMap(_.value)

  def addressOperands: Set[AddressOperandInfo] = location.addressOperands
}
