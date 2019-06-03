package org.werner.absynt.x86.operands.memoryaccess

import org.werner.absynt.x86.operands._

abstract class IndirectMemoryLocation(val registerOrMemoryModeCode: Byte, displacement: Option[ImmediateValue with ByteWordDoubleSize] = None,
                                      segment: SegmentRegister)
  extends MemoryLocation(displacement, segment) {

  val modValue: Byte = {
    displacement match {
      case None => 0x00
      case Some(d) if d.isInstanceOf[ByteSize] => 0x01
      case Some(d) if d.isInstanceOf[WordDoubleSize] => 0x02
    }
  }
}
