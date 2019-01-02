package assembler.x86.operations

import assembler.x86.operands.ValueSize
import assembler.x86.operands.memoryaccess.{MemoryLocation => MemoryLocationType}
import assembler.x86.operations.OperandInfo.OperandOrder._

trait MemoryLocation[Size<:ValueSize] extends DisplacementBytes {

  self: X86Operation =>
  def location: MemoryLocationType with Size
  def offsetOrder: OperandOrder

  override protected def displacementInit(): Unit =
    addOperand(OperandInfo.memoryOffset(location, offsetOrder))

  override def displacementBytes: Seq[Byte] = location.displacement.toSeq.flatMap(_.value)

  def addressOperands: Set[AddressOperandInfo] = location.addressOperands
}
