package assembler.x86.operands.memoryaccess

import assembler.ListExtensions.ListToImmediate
import assembler.x86.operands._
import assembler.x86.operations.AddressOperandInfo

sealed class MemoryAddress private(address: ImmediateValue, segment: SegmentRegister = Register.DS)
  extends MemoryLocation(Some(address), segment) with ModRMEncodableOperand {

  override val modValue: Byte = 0x00.toByte

  override val registerOrMemoryModeCode: Byte = if (address.isInstanceOf[WordSize]) 0x06.toByte else 0x05.toByte
  final override val defaultSegment: SegmentRegister = Register.DS

  override val addressOperands: Set[AddressOperandInfo] = Set(AddressOperandInfo.rmDisplacement(address, segmentOverride))

  override def getExtendedBytes(rValue: Byte): Seq[Byte] = super.getExtendedBytes(rValue) ++ address.value

  override def toString = s"$segmentPrefix[${address.value.decimalString}]"
}

object MemoryAddress {
  abstract class MemoryAddressForSize[Size<:ValueSize] {
    def instance(address: ImmediateValue, segment: SegmentRegister = Register.DS): MemoryAddress with Size
  }

  implicit def memoryAddressForByteSize: MemoryAddressForSize[ByteSize] =
    (address: ImmediateValue, segment: SegmentRegister) => new MemoryAddress(address, segment) with ByteSize

  implicit def memoryAddressForWordSize: MemoryAddressForSize[WordSize] =
    (address: ImmediateValue, segment: SegmentRegister) => new MemoryAddress(address, segment) with WordSize

  implicit def memoryAddressForDoubleWordSize: MemoryAddressForSize[DoubleWordSize] =
    (address: ImmediateValue, segment: SegmentRegister) => new MemoryAddress(address, segment) with DoubleWordSize

  implicit def memoryAddressForQuadWordSize: MemoryAddressForSize[QuadWordSize] =
    (address: ImmediateValue, segment: SegmentRegister) => new MemoryAddress(address, segment) with QuadWordSize

  def apply[Size<:ValueSize:MemoryAddressForSize](address: ImmediateValue, segment: SegmentRegister = Register.DS): MemoryAddress with Size =
    implicitly[MemoryAddressForSize[Size]].instance(address, segment)
}