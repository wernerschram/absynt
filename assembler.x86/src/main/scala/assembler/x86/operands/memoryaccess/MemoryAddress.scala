package assembler.x86.operands.memoryaccess

import assembler.ListExtensions.ListToImmediate
import assembler.x86.operands._
import assembler.x86.operations.AddressOperandInfo

sealed class MemoryAddress private(address: ImmediateValue with ValueSize, segment: SegmentRegister = Register.Segment.Data)
  extends MemoryLocation(Some(address), segment) with ModRMEncodableOperand {
  self: ValueSize =>

  override val modValue: Byte = 0x00.toByte

  override val registerOrMemoryModeCode: Byte = if (address.isInstanceOf[WordSize]) 0x06.toByte else 0x05.toByte
  final override val defaultSegment: SegmentRegister = Register.Segment.Data

  override val addressOperands: Set[AddressOperandInfo] = Set(AddressOperandInfo.rmDisplacement(address, segmentOverride))

  override def getExtendedBytes(rValue: Byte): Seq[Byte] = super.getExtendedBytes(rValue) ++ address.value

  override def toString = s"$sizeName PTR $segmentPrefix[${address.value.decimalString}]"
}

object MemoryAddress {
  abstract class MemoryAddressForSize[Size<:ValueSize] {
    def instance(address: ImmediateValue with ValueSize, segment: SegmentRegister = Register.Segment.Data): MemoryAddress with Size
  }

  def apply[Size<:ValueSize:MemoryAddressForSize](address: ImmediateValue with ValueSize, segment: SegmentRegister = Register.Segment.Data): MemoryAddress with Size =
    implicitly[MemoryAddressForSize[Size]].instance(address, segment)

  trait I8086Implicits {
    implicit def ByteMemoryAddress: MemoryAddressForSize[ByteSize] =
      (address: ImmediateValue with ValueSize, segment: SegmentRegister) => new MemoryAddress(address, segment) with ByteSize
    implicit def WordMemoryAddress: MemoryAddressForSize[WordSize] =
      (address: ImmediateValue with ValueSize, segment: SegmentRegister) => new MemoryAddress(address, segment) with WordSize
  }

  trait I386Implicits {
    implicit def DoubleWordMemoryAddress: MemoryAddressForSize[DoubleWordSize] =
      (address: ImmediateValue with ValueSize, segment: SegmentRegister) => new MemoryAddress(address, segment) with DoubleWordSize
  }

  trait X64Implicits {
    implicit def QuadWordMemoryAddress: MemoryAddressForSize[QuadWordSize] =
      (address: ImmediateValue with ValueSize, segment: SegmentRegister) => new MemoryAddress(address, segment) with QuadWordSize
  }
}