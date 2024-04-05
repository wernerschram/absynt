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

import org.werner.absynt.ListExtensions.*
import org.werner.absynt.x86.operands.*
import org.werner.absynt.x86.operations.{AddressOperandInfo, AddressSizePrefixRequirement}

sealed class MemoryAddress private(address: ImmediateValue[?] & ValueSize, segment: SegmentRegister = Segment.Data)(using addressSizePrefixRequirement: AddressSizePrefixRequirement)
  extends MemoryLocation(Some(address), segment) with ModRMEncodableOperand {
  self: ValueSize =>

  override val modValue: Byte = 0x00.toByte

  // TODO: WordSize is not valid in Long mode
  override val registerOrMemoryModeCode: Byte = if address.isInstanceOf[WordSize] then 0x06.toByte else 0x05.toByte
  final override val defaultSegment: SegmentRegister = Segment.Data

  override def addressOperands(using addressSizePrefixRequirement: AddressSizePrefixRequirement): Set[AddressOperandInfo] =
    Set(AddressOperandInfo.rmDisplacement(address, segmentOverride))

  override def getExtendedBytes(rValue: Byte): Seq[Byte] = super.getExtendedBytes(rValue) ++ address.encodedValue

  override def toString: String = s"$sizeName PTR $segmentPrefix[${address.encodedValue.decimalString}]"
}

object MemoryAddress {
  trait MemoryAddressForSize[Size<:ValueSize]: 
    extension(address: ImmediateValue[?] & ValueSize) def instance(segment: SegmentRegister = Segment.Data)(using addressSizePrefixRequirement: AddressSizePrefixRequirement): MemoryAddress & Size
  

  def apply[Size<:ValueSize:MemoryAddressForSize](address: ImmediateValue[?] & ValueSize, segment: SegmentRegister = Segment.Data)(using addressSizePrefixRequirement: AddressSizePrefixRequirement): MemoryAddress & Size =
    address.instance(segment)

  trait I8086Implicits {
    
    given MemoryAddressForSize[ByteSize] with
      extension (address: ImmediateValue[?] & ValueSize) override def instance(segment: SegmentRegister)(using addressSizePrefixRequirement: AddressSizePrefixRequirement): MemoryAddress & ByteSize =
        new MemoryAddress(address, segment) with ByteSize
   
    given MemoryAddressForSize[WordSize] with
      extension (address: ImmediateValue[?] & ValueSize) override def instance(segment: SegmentRegister)(using addressSizePrefixRequirement: AddressSizePrefixRequirement): MemoryAddress & WordSize =
        new MemoryAddress(address, segment) with WordSize
  }

  trait I386Implicits {
    
    given MemoryAddressForSize[DoubleWordSize] with
      extension (address: ImmediateValue[?] & ValueSize) override def instance(segment: SegmentRegister)(using addressSizePrefixRequirement: AddressSizePrefixRequirement): MemoryAddress & DoubleWordSize = 
        new MemoryAddress(address, segment) with DoubleWordSize
  }

  trait X64Implicits {
    
    given MemoryAddressForSize[QuadWordSize] with
      extension (address: ImmediateValue[?] & ValueSize) override def instance(segment: SegmentRegister)(using addressSizePrefixRequirement: AddressSizePrefixRequirement): MemoryAddress & QuadWordSize = 
        new MemoryAddress(address, segment) with QuadWordSize
  }
}