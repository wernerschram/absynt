package assembler.x86.operations

import assembler.x86.operands.SegmentRegister
import assembler.x86.operands.memoryaccess.{MemoryLocation => MemoryLocationType}
import assembler.x86.operations.OperandInfo.OperandOrder._

trait MemoryLocation extends X86Operation {

  self: X86Operation =>
  def location: MemoryLocationType
  def offsetOrder: OperandOrder

  abstract override def operands: Seq[OperandInfo] =
    super.operands :+ OperandInfo.memoryOffset(location, offsetOrder)

  def addressOperands: Seq[AddressOperandInfo] =
    location.addressOperands

  override def segmentOverride: Option[SegmentRegister] = super.segmentOverride match {
    case register: Some[SegmentRegister] => register
    case None => location.segmentOverride
  }

  abstract override def encodeByte: Seq[Byte] =
    super.encodeByte ++ location.displacement.toSeq.flatMap(_.value)
}
