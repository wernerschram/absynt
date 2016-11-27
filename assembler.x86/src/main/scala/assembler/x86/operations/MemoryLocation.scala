package assembler.x86.operations

import assembler.memory.MemoryPage
import assembler.x86.ParameterPosition
import assembler.x86.operands.SegmentRegister
import assembler.x86.operands.memoryaccess.{ MemoryLocation => MemoryLocationType }
import assembler.x86.operands.OperandSize

trait MemoryLocation extends X86Operation {

  self: X86Operation =>
  def location: MemoryLocationType

  abstract override def operands = super.operands ::: location :: Nil

  abstract override def addressSize: Option[Int] = super.addressSize match {
    case None => Some(location.addressSize)
    case default => super.addressSize
  }

  override def segmentOverride = super.segmentOverride match {
    case register: Some[SegmentRegister] => register
    case None => location.getSegmentOverride
  }

  abstract override def encodeByte()(implicit page: MemoryPage): List[Byte] =
    super.encodeByte() ::: location.displacement
}
