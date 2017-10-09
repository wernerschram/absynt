package assembler.x86

import assembler.x86.operands.memoryaccess._

sealed abstract class ProcessorMode

trait OffsetFactory[OffsetType] {
  implicit def offset(offsetValue: Long): OffsetType
}


sealed abstract class ProcessorModeWithOffset[OffsetType <: X86Offset] extends ProcessorMode with OffsetFactory[OffsetType] {
  implicit val processorMode: ProcessorModeWithOffset[OffsetType] = this
}

object ProcessorMode {

  object Real extends ProcessorModeWithOffset[RealOffset] {
    override implicit def offset(offset: Long): RealOffset = new RealOffset(offset)
  }
  object Protected extends ProcessorModeWithOffset[ProtectedOffset] {
    override implicit def offset(offset: Long): ProtectedOffset = new ProtectedOffset(offset)
  }
  object Long extends ProcessorModeWithOffset[ProtectedOffset] {
    override implicit def offset(offset: Long): ProtectedOffset = new ProtectedOffset(offset)
  }
}
