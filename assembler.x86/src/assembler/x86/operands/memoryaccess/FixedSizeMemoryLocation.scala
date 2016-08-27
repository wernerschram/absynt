package assembler.x86.operands.memoryaccess

import assembler.x86.ParameterPosition
import assembler.x86.operands.FixedSizeEncodableOperand
import assembler.x86.operands.registers.Register
import assembler.x86.operands.registers.SegmentRegister

final class FixedSizeMemoryLocation private (location: MemoryLocation, val operandByteSize: Int, segment: SegmentRegister = Register.DS) 
    extends MemoryLocation(location.displacement, segment, location.addressSize) with FixedSizeEncodableOperand {
  val defaultSegment: SegmentRegister = location.defaultSegment

  override def getExtendedBytes(rValue: Byte): List[Byte] = location.getExtendedBytes(rValue)

  override def getRexRequirements(position: ParameterPosition) = location.getRexRequirements(position)
}

object FixedSizeMemoryLocation {
}