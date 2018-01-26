package assembler.x86.operands.memoryaccess

import assembler.ListExtensions.ListToImmediate
import assembler.x86.operands._

sealed class MemoryAddress private(address: Displacement, segment: SegmentRegister = Register.DS)
  extends MemoryLocation(address, segment, ValueSize.sizeOfValue(address.encode.size)) with ModRMEncodableOperand {

  override val modValue: Byte = 0x00.toByte

  override val registerOrMemoryModeCode: Byte = if (address.encode.lengthCompare(2) == 0) 0x06.toByte else 0x05.toByte
  override val defaultSegment: SegmentRegister = Register.DS

  override def getExtendedBytes(rValue: Byte): Seq[Byte] = super.getExtendedBytes(rValue) ++ displacement.encode

  override def toString = s"$segmentPrefix[${address.encode.decimalString}]"
}

object MemoryAddress {

  def apply(address: Displacement, segment: SegmentRegister = Register.DS) =
    new MemoryAddress(address, segment)

  def withSize(address: Displacement, segment: SegmentRegister = Register.DS)(size: ValueSize): MemoryAddress with FixedSizeOperand =
    new MemoryAddress(address, segment) with FixedSizeOperand {
      override val operandByteSize: OperandSize = size
    }
}