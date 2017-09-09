package assembler.x86.operations

import assembler.x86.operands.memoryaccess.{MemoryLocation => MemoryLocationType}
import assembler.x86.operands.{Operand, OperandSize, SegmentRegister}

trait MemoryLocation extends X86Operation {

  self: X86Operation =>
  def location: MemoryLocationType

  abstract override def operands: List[Operand] = super.operands ::: location :: Nil

  abstract override def addressSize: OperandSize = location.addressSize

  override def segmentOverride: Option[SegmentRegister] = super.segmentOverride match {
    case register: Some[SegmentRegister] => register
    case None => location.segmentOverride
  }

  abstract override def encodeByte: List[Byte] =
    super.encodeByte ::: location.displacement
}
