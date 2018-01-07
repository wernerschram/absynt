package assembler.x86.operands.memoryaccess

import assembler.x86.operands.{OperandSize, SegmentRegister}

abstract class IndirectMemoryLocation(val registerOrMemoryModeCode: Byte, displacement: Seq[Byte] = Seq.empty[Byte],
                                      addressSize: OperandSize, segment: SegmentRegister)
  extends MemoryLocation(displacement, segment, addressSize) {

  val modValue: Byte = IndirectMemoryLocation.getModValue(displacement)
}

object IndirectMemoryLocation {
  private def getModValue(displacement: Seq[Byte]): Byte = {
    assume((0 :: 1 :: 2 :: 4 :: Nil).contains(displacement.size))
    displacement.length match {
      case 0 => 0x00
      case 1 => 0x01
      case 2 | 4 => 0x02
    }
  }
}