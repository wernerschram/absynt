package assembler.x86.operands.memoryaccess

import assembler.ListExtensions.ListToImmediate
import assembler.x86.operands._

sealed class MemoryAddress private(address: Seq[Byte], segment: SegmentRegister = Register.DS)
  extends MemoryLocation(address, segment, ValueSize.sizeOfValue(address.size)) with ModRMEncodableOperand {

  override val modValue: Byte = 0x00.toByte

  override val registerOrMemoryModeCode: Byte = if (address.lengthCompare(2) == 0) 0x06.toByte else 0x05.toByte
  override val defaultSegment: SegmentRegister = Register.DS

  override def getExtendedBytes(rValue: Byte): List[Byte] = super.getExtendedBytes(rValue) ++ displacement

  override def toString = s"$segmentPrefix[${address.decimalString}]"
}

object MemoryAddress {

  def apply(address: Seq[Byte], segment: SegmentRegister = Register.DS) =
    new MemoryAddress(address, segment)

  def byteSize(address: Seq[Byte], segment: SegmentRegister = Register.DS) =
    FixedSizeMemoryAddress(address, segment, ValueSize.Byte)

  def wordSize(address: Seq[Byte], segment: SegmentRegister = Register.DS) =
    FixedSizeMemoryAddress(address, segment, ValueSize.Word)

  def doubleWordSize(address: Seq[Byte], segment: SegmentRegister = Register.DS) =
    FixedSizeMemoryAddress(address, segment, ValueSize.DoubleWord)

  def quadWordSize(address: Seq[Byte], segment: SegmentRegister = Register.DS) =
    FixedSizeMemoryAddress(address, segment, ValueSize.QuadWord)

  final class FixedSizeMemoryAddress private(address: Seq[Byte], segment: SegmentRegister = Register.DS, val operandByteSize: OperandSize)
    extends MemoryAddress(address, segment) with ModRMEncodableOperand with FixedSizeOperand {
  }

  private object FixedSizeMemoryAddress {
    def apply(address: Seq[Byte], segment: SegmentRegister = Register.DS, operandByteSize: OperandSize) =
      new FixedSizeMemoryAddress(address, segment, operandByteSize)
  }

}