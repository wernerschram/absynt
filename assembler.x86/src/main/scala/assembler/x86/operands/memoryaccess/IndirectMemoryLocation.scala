package assembler.x86.operands.memoryaccess

import assembler.x86.operands.registers.SegmentRegister

abstract class IndirectMemoryLocation(
    val registerOrMemoryModeCode: Byte, displacement: List[Byte] = List.empty[Byte], addressSize: Int, segment: SegmentRegister) 
      extends MemoryLocation(displacement, segment, addressSize) {

  val modValue = IndirectMemoryLocation.getModValue(displacement)
}

object IndirectMemoryLocation {
  private def getModValue(displacement: List[Byte]): Byte =
    displacement.length match {
      case 0 => 0x00
      case 1 => 0x01
      case 2 | 4 => 0x02
      case _ => throw new Exception // TODO: replace with correct exception
    }
}