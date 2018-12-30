package assembler.x86.operations

import assembler.x86.operands.memoryaccess.{MemoryLocation => MemoryLocationType}
import assembler.x86.operations.OperandInfo.OperandOrder._

trait MemoryLocation extends DisplacementBytes {

  self: X86Operation =>
  def location: MemoryLocationType
  def offsetOrder: OperandOrder

  override protected def displacementInit(): Unit =
    addOperand(OperandInfo.memoryOffset(location, offsetOrder))

  override def displacementBytes: Seq[Byte] = location.displacement.toSeq.flatMap(_.value)

  def addressOperands: Set[AddressOperandInfo] = location.addressOperands
}
