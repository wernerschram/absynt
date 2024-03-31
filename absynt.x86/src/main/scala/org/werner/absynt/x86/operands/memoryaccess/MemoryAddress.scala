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
import org.werner.absynt.x86.operations.{AddressOperandInfo, AddressSizePrefixRequirement}

sealed class MemoryAddress private(address: ImmediateValue[_] with ValueSize, segment: SegmentRegister = Segment.Data)(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement)
  extends MemoryLocation(Some(address), segment) with ModRMEncodableOperand {
  self: ValueSize =>

  override val modValue: Byte = 0x00.toByte

  // TODO: WordSize is not valid in Long mode
  override val registerOrMemoryModeCode: Byte = if (address.isInstanceOf[WordSize]) 0x06.toByte else 0x05.toByte
  final override val defaultSegment: SegmentRegister = Segment.Data

  override def addressOperands(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): Set[AddressOperandInfo] =
    Set(AddressOperandInfo.rmDisplacement(address, segmentOverride))

  override def getExtendedBytes(rValue: Byte): Seq[Byte] = super.getExtendedBytes(rValue) ++ address.encodedValue

  override def toString: String = s"$sizeName PTR $segmentPrefix[${address.encodedValue.decimalString}]"
}

object MemoryAddress {
  abstract class MemoryAddressForSize[Size<:ValueSize] {
    def instance(address: ImmediateValue[_] with ValueSize, segment: SegmentRegister = Segment.Data): MemoryAddress with Size
  }

  def apply[Size<:ValueSize:MemoryAddressForSize](address: ImmediateValue[_] with ValueSize, segment: SegmentRegister = Segment.Data): MemoryAddress with Size =
    implicitly[MemoryAddressForSize[Size]].instance(address, segment)

  trait I8086Implicits {
    implicit def ByteMemoryAddress(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): MemoryAddressForSize[ByteSize] =
      (address: ImmediateValue[_] with ValueSize, segment: SegmentRegister) => new MemoryAddress(address, segment) with ByteSize
    implicit def WordMemoryAddress(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): MemoryAddressForSize[WordSize] =
      (address: ImmediateValue[_] with ValueSize, segment: SegmentRegister) => new MemoryAddress(address, segment) with WordSize
  }

  trait I386Implicits {
    implicit def DoubleWordMemoryAddress(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): MemoryAddressForSize[DoubleWordSize] =
      (address: ImmediateValue[_] with ValueSize, segment: SegmentRegister) => new MemoryAddress(address, segment) with DoubleWordSize
  }

  trait X64Implicits {
    implicit def QuadWordMemoryAddress(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): MemoryAddressForSize[QuadWordSize] =
      (address: ImmediateValue[_] with ValueSize, segment: SegmentRegister) => new MemoryAddress(address, segment) with QuadWordSize
  }
}