package assembler.x86.operations

import assembler.x86.operands.memoryaccess.{MemoryLocation => MemoryLocationType}
import assembler.x86.operands.{OperandSize, SegmentRegister}

trait MemoryLocation extends X86Operation {

  self: X86Operation =>
  def location: MemoryLocationType

  abstract override def operands: Seq[OperandInfo] = super.operands :+ OperandInfo.memoryOffset(location)

  abstract override def addressSize: OperandSize = location.addressSize

  override def segmentOverride: Option[SegmentRegister] = super.segmentOverride match {
    case register: Some[SegmentRegister] => register
    case None => location.segmentOverride
  }

  abstract override def encodeByte: Seq[Byte] =
    super.encodeByte ++ location.displacement.encode
}
