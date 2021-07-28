/*
 * Copyright 2019 Werner Schram
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use
 * this file except in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */

package org.werner.absynt.x86.operands.memoryaccess

import org.werner.absynt.ListExtensions.ListToImmediate
import org.werner.absynt.x86.operands._
import org.werner.absynt.x86.operands.registers.{Segment, SegmentRegister}
import org.werner.absynt.x86.operations.{AddressOperandInfo, AddressSizePrefixRequirement}

abstract sealed class MemoryAddress protected(address: Long, segment: SegmentRegister = Segment.Data)(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement)
  extends MemoryLocation(
    if (address.toShort == address)
      Some(ImmediateValue.wordImmediate(address.toShort))
    else if (address.toInt == address)
      Some(ImmediateValue.doubleWordImmediate(address.toInt))
    else
      Some(ImmediateValue.quadWordImmediate(address)),
    segment) with ModRMEncodableOperand {
  self: ValueSize =>

  override val modValue: Byte = 0x00.toByte

  protected val addressImmediate: ImmediateValue[_] with WordDoubleQuadSize =
    if (address.toShort == address)
      ImmediateValue.wordImmediate(address.toShort)
    else if (address.toInt == address)
      ImmediateValue.doubleWordImmediate(address.toInt)
    else
    ImmediateValue.quadWordImmediate(address)

  // TODO: WordSize is not valid in Long mode
  override val registerOrMemoryModeCode: Byte

  final override val defaultSegment: SegmentRegister = Segment.Data

  override def addressOperands(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): Set[AddressOperandInfo] =
    Set(AddressOperandInfo.rmDisplacement(addressImmediate, segmentOverride))

  override def toString = s"$sizeName PTR $segmentPrefix[${addressImmediate.encodedValue.decimalString}]"
}

sealed class RealProtectedModeMemoryAddress protected(address: Long, segment: SegmentRegister = Segment.Data)(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement)
  extends MemoryAddress(address, segment) {
    self: ValueSize =>

  protected override val addressImmediate: ImmediateValue[_] with WordDoubleQuadSize =
    if (address.toShort == address)
      ImmediateValue.wordImmediate(address.toShort)
    else
      ImmediateValue.doubleWordImmediate(address.toInt)

  override def getExtendedBytes(rValue: Byte): Seq[Byte] = super.getExtendedBytes(rValue) ++ addressImmediate.encodedValue

  override val registerOrMemoryModeCode: Byte = if (addressImmediate.isInstanceOf[WordSize]) 0x06.toByte else 0x05.toByte

}

class LongModeMemoryAddress protected(address: Long, segment: SegmentRegister = Segment.Data)(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement)
  extends MemoryAddress(address, segment) {
    self: ValueSize =>
  override val registerOrMemoryModeCode: Byte = 0x04.toByte

  protected override val addressImmediate: ImmediateValue[_] with WordDoubleQuadSize =
    if (address.toInt == address)
      ImmediateValue.doubleWordImmediate(address.toInt)
    else
      ImmediateValue.quadWordImmediate(address)

  private val SIB = Seq(0x25.toByte)

  override def getExtendedBytes(rValue: Byte): Seq[Byte] = super.getExtendedBytes(rValue) ++ SIB ++ addressImmediate.encodedValue
}

object MemoryAddress {
  abstract class MemoryAddressForSize[Size<:ValueSize] {
    def instance(address: Long, segment: SegmentRegister = Segment.Data): MemoryAddress with Size
  }

  trait I8086Implicits {
    implicit def ByteMemoryAddress(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): MemoryAddressForSize[ByteSize] =
      (address: Long, segment: SegmentRegister) => new RealProtectedModeMemoryAddress(address, segment) with ByteSize

    implicit def WordMemoryAddress(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): MemoryAddressForSize[WordSize] =
      (address: Long, segment: SegmentRegister) => new RealProtectedModeMemoryAddress(address, segment) with WordSize
  }

  trait I386Implicits extends I8086Implicits {
    implicit def DoubleWordMemoryAddress(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): MemoryAddressForSize[DoubleWordSize] =
      (address: Long, segment: SegmentRegister) => new RealProtectedModeMemoryAddress(address, segment) with DoubleWordSize
  }

  trait X64Implicits extends I386Implicits {
    override implicit def ByteMemoryAddress(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): MemoryAddressForSize[ByteSize] =
      (address: Long, segment: SegmentRegister) => new LongModeMemoryAddress(address, segment) with ByteSize

    override implicit def WordMemoryAddress(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): MemoryAddressForSize[WordSize] =
      (address: Long, segment: SegmentRegister) => new LongModeMemoryAddress(address, segment) with WordSize

    override implicit def DoubleWordMemoryAddress(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): MemoryAddressForSize[DoubleWordSize] =
      (address: Long, segment: SegmentRegister) => new LongModeMemoryAddress(address, segment) with DoubleWordSize

    implicit def QuadWordMemoryAddress(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): MemoryAddressForSize[QuadWordSize] =
      (address: Long, segment: SegmentRegister) => new LongModeMemoryAddress(address, segment) with QuadWordSize
  }
}