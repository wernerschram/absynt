package org.werner.absynt.x86.operands.memoryaccess

import org.werner.absynt.ListExtensions.ListToImmediate
import org.werner.absynt.x86.operands._
import org.werner.absynt.x86.operations.{AddressOperandInfo, AddressSizePrefixRequirement}
import org.werner.absynt.x86.operations.{AddressOperandInfo, AddressSizePrefixRequirement}

sealed class MemoryAddress private(address: ImmediateValue with ValueSize, segment: SegmentRegister = Segment.Data)(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement)
  extends MemoryLocation(Some(address), segment) with ModRMEncodableOperand {
  self: ValueSize =>

  override val modValue: Byte = 0x00.toByte

  override val registerOrMemoryModeCode: Byte = if (address.isInstanceOf[WordSize]) 0x06.toByte else 0x05.toByte
  final override val defaultSegment: SegmentRegister = Segment.Data

  override def addressOperands: Set[AddressOperandInfo] =
    Set(AddressOperandInfo.rmDisplacement(address, segmentOverride))

  override def getExtendedBytes(rValue: Byte): Seq[Byte] = super.getExtendedBytes(rValue) ++ address.value

  override def toString = s"$sizeName PTR $segmentPrefix[${address.value.decimalString}]"
}

object MemoryAddress {
  abstract class MemoryAddressForSize[Size<:ValueSize] {
    def instance(address: ImmediateValue with ValueSize, segment: SegmentRegister = Segment.Data): MemoryAddress with Size
  }

  def apply[Size<:ValueSize:MemoryAddressForSize](address: ImmediateValue with ValueSize, segment: SegmentRegister = Segment.Data): MemoryAddress with Size =
    implicitly[MemoryAddressForSize[Size]].instance(address, segment)

  trait I8086Implicits {
    implicit def ByteMemoryAddress(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): MemoryAddressForSize[ByteSize] =
      (address: ImmediateValue with ValueSize, segment: SegmentRegister) => new MemoryAddress(address, segment) with ByteSize
    implicit def WordMemoryAddress(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): MemoryAddressForSize[WordSize] =
      (address: ImmediateValue with ValueSize, segment: SegmentRegister) => new MemoryAddress(address, segment) with WordSize
  }

  trait I386Implicits {
    implicit def DoubleWordMemoryAddress(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): MemoryAddressForSize[DoubleWordSize] =
      (address: ImmediateValue with ValueSize, segment: SegmentRegister) => new MemoryAddress(address, segment) with DoubleWordSize
  }

  trait X64Implicits {
    implicit def QuadWordMemoryAddress(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): MemoryAddressForSize[QuadWordSize] =
      (address: ImmediateValue with ValueSize, segment: SegmentRegister) => new MemoryAddress(address, segment) with QuadWordSize
  }
}