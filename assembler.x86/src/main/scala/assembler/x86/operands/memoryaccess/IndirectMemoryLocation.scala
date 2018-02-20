package assembler.x86.operands.memoryaccess

import assembler.x86.operands.{ImmediateValue, SegmentRegister}

abstract class IndirectMemoryLocation(val registerOrMemoryModeCode: Byte, displacement: Option[ImmediateValue] = None,
                                      segment: SegmentRegister)
  extends MemoryLocation(displacement, segment) {

  val modValue: Byte = {
    displacement match {
      case None => 0x00
      case Some(d) if d.value.lengthCompare(1) == 0 => 0x01
      case Some(d) if d.value.lengthCompare(2) == 0 => 0x02
      case Some(d) if d.value.lengthCompare(4) == 0 => 0x02
      case _ => throw new AssertionError
    }
  }
}
