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

import org.werner.absynt.ListExtensions._
import org.werner.absynt.x86.operands.{ModRMEncodableOperand, _}
import org.werner.absynt.x86.operations.{AddressOperandInfo, AddressSizePrefixRequirement}

//TODO: of Page 2-7 of the intel software development manual (325383-sdm-vol-2abcd.pdf) Notes: 1. and Scaled Index == none are not implemented
sealed class SIBMemoryLocation(
    val index: GeneralPurposeRegister with SIBIndexRegister with DoubleQuadSize,
    val base: GeneralPurposeRegister with SIBBaseRegister with DoubleQuadSize,
    displacement: Option[ImmediateValue[_] with ByteWordDoubleSize] = None, val scale: Int, segment: SegmentRegister)
  extends IndirectMemoryLocation(0x04, displacement, segment) with ModRMEncodableOperand {

  self: ValueSize =>

//  assume(index sizeEquals base)
  assume((1 :: 2 :: 4 :: 8 :: Nil).contains(scale))

  override val defaultSegment: SegmentRegister = index.defaultSIBSegment
  val baseCode: Byte = base.SIBBaseCode
  val indexCode: Byte = index.SIBIndexCode

  override def addressOperands(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): Set[AddressOperandInfo] =
    Set(AddressOperandInfo.SIBBase(base), AddressOperandInfo.SIBIndex(index, segmentOverride))

  override def getExtendedBytes(rValue: Byte): Seq[Byte] = super.getExtendedBytes(rValue) ++ (getSIB +: displacement.toSeq.flatMap(_.encodedValue))

  def getSIB: Byte = {
    val scaleCode = scale match {
      case 1 => 0x0
      case 2 => 0x1
      case 4 => 0x2
      case 8 => 0x3
    }
    ((scaleCode << 6) | (indexCode << 3) | baseCode).toByte
  }

  override def toString: String = s"$sizeName PTR $segmentPrefix[$base+$index$scaleString$displacementString]"

  private def scaleString = s"*$scale"

  private def displacementString = displacement match {
    case None => ""
    case Some(d) => s"+${d.encodedValue.decimalString}"
  }
}

object SIBMemoryLocation {

  trait I386Operations {
    abstract class SIBForSize[Size<:ValueSize]() {
      def instance(index: GeneralPurposeRegister with SIBIndexRegister with DoubleWordSize, base: GeneralPurposeRegister with SIBBaseRegister with DoubleWordSize, displacement: Option[ImmediateValue[_] with ByteWordDoubleSize], scale: Int, segmentOverride: SegmentRegister): SIBMemoryLocation with Size
    }

    implicit def SIBforByteSize: SIBForSize[ByteSize] =
      (index: GeneralPurposeRegister with SIBIndexRegister with DoubleWordSize, base: GeneralPurposeRegister with SIBBaseRegister with DoubleWordSize, displacement: Option[ImmediateValue[_] with ByteWordDoubleSize], scale: Int, segmentOverride: SegmentRegister) =>
        new SIBMemoryLocation(index, base, displacement, scale, segmentOverride) with ByteSize

    implicit def SIBforWordSize: SIBForSize[WordSize] =
      (index: GeneralPurposeRegister with SIBIndexRegister with DoubleWordSize, base: GeneralPurposeRegister with SIBBaseRegister with DoubleWordSize, displacement: Option[ImmediateValue[_] with ByteWordDoubleSize], scale: Int, segmentOverride: SegmentRegister) =>
        new SIBMemoryLocation(index, base, displacement, scale, segmentOverride) with WordSize

    implicit def SIBforDoubleWordSize: SIBForSize[DoubleWordSize] =
      (index: GeneralPurposeRegister with SIBIndexRegister with DoubleWordSize, base: GeneralPurposeRegister with SIBBaseRegister with DoubleWordSize, displacement: Option[ImmediateValue[_] with ByteWordDoubleSize], scale: Int, segmentOverride: SegmentRegister) =>
        new SIBMemoryLocation(index, base, displacement, scale, segmentOverride) with DoubleWordSize

    object SIBMemoryLocation {
      def apply[Size <: ValueSize : SIBForSize](index: GeneralPurposeRegister with SIBIndexRegister with DoubleWordSize, base: GeneralPurposeRegister with SIBBaseRegister with DoubleWordSize, scale: Int): SIBMemoryLocation with Size =
        implicitly[SIBForSize[Size]].instance(index, base, None, scale, index.defaultSIBSegment)

      def apply[Size <: ValueSize : SIBForSize](index: GeneralPurposeRegister with SIBIndexRegister with DoubleWordSize, base: GeneralPurposeRegister with SIBBaseRegister with DoubleWordSize, displacement: ImmediateValue[_] with ByteWordDoubleSize, scale: Int): SIBMemoryLocation with Size =
        implicitly[SIBForSize[Size]].instance(index, base, Some(displacement), scale, index.defaultSIBSegment)

      object withSegmentOverride {
        def apply[Size <: ValueSize : SIBForSize](index: GeneralPurposeRegister with SIBIndexRegister with DoubleWordSize, base: GeneralPurposeRegister with SIBBaseRegister with DoubleWordSize, scale: Int, segment: SegmentRegister): SIBMemoryLocation with Size =
          implicitly[SIBForSize[Size]].instance(index, base, None, scale, segment)

        def apply[Size <: ValueSize : SIBForSize](index: GeneralPurposeRegister with SIBIndexRegister with DoubleWordSize, base: GeneralPurposeRegister with SIBBaseRegister with DoubleWordSize, displacement: ImmediateValue[_] with ByteWordDoubleSize, scale: Int, segment: SegmentRegister): SIBMemoryLocation with Size =
          implicitly[SIBForSize[Size]].instance(index, base, Some(displacement), scale, segment)
      }

    }
  }

  trait LongOperations {
    abstract class SIBForSize[Size<:ValueSize]() {
      def instance(index: GeneralPurposeRegister with SIBIndexRegister with DoubleQuadSize, base: GeneralPurposeRegister with SIBBaseRegister with DoubleQuadSize, displacement: Option[ImmediateValue[_] with ByteWordDoubleSize], scale: Int, segmentOverride: SegmentRegister): SIBMemoryLocation with Size
    }

    implicit def SIBforByteSize: SIBForSize[ByteSize] =
      (index: GeneralPurposeRegister with SIBIndexRegister with DoubleQuadSize, base: GeneralPurposeRegister with SIBBaseRegister with DoubleQuadSize, displacement: Option[ImmediateValue[_] with ByteWordDoubleSize], scale: Int, segmentOverride: SegmentRegister) =>
        new SIBMemoryLocation(index, base, displacement, scale, segmentOverride) with ByteSize

    implicit def SIBforWordSize: SIBForSize[WordSize] =
      (index: GeneralPurposeRegister with SIBIndexRegister with DoubleQuadSize, base: GeneralPurposeRegister with SIBBaseRegister with DoubleQuadSize, displacement: Option[ImmediateValue[_] with ByteWordDoubleSize], scale: Int, segmentOverride: SegmentRegister) =>
        new SIBMemoryLocation(index, base, displacement, scale, segmentOverride) with WordSize

    implicit def SIBforDoubleWordSize: SIBForSize[DoubleWordSize] =
      (index: GeneralPurposeRegister with SIBIndexRegister with DoubleQuadSize, base: GeneralPurposeRegister with SIBBaseRegister with DoubleQuadSize, displacement: Option[ImmediateValue[_] with ByteWordDoubleSize], scale: Int, segmentOverride: SegmentRegister) =>
        new SIBMemoryLocation(index, base, displacement, scale, segmentOverride) with DoubleWordSize

    implicit def SIBforQuadWordSize: SIBForSize[QuadWordSize] =
      (index: GeneralPurposeRegister with SIBIndexRegister with DoubleQuadSize, base: GeneralPurposeRegister with SIBBaseRegister with DoubleQuadSize, displacement: Option[ImmediateValue[_] with ByteWordDoubleSize], scale: Int, segmentOverride: SegmentRegister) =>
        new SIBMemoryLocation(index, base, displacement, scale, segmentOverride) with QuadWordSize

    object SIBMemoryLocation {
      def apply[Size <: ValueSize : SIBForSize](index: GeneralPurposeRegister with SIBIndexRegister with DoubleQuadSize, base: GeneralPurposeRegister with SIBBaseRegister with DoubleQuadSize, scale: Int): SIBMemoryLocation with Size =
        implicitly[SIBForSize[Size]].instance(index, base, None, scale, index.defaultSIBSegment)

      def apply[Size <: ValueSize : SIBForSize](index: GeneralPurposeRegister with SIBIndexRegister with DoubleQuadSize, base: GeneralPurposeRegister with SIBBaseRegister with DoubleQuadSize, displacement: ImmediateValue[_] with ByteWordDoubleSize, scale: Int): SIBMemoryLocation with Size =
        implicitly[SIBForSize[Size]].instance(index, base, Some(displacement), scale, index.defaultSIBSegment)

      object withSegmentOverride {
        def apply[Size <: ValueSize : SIBForSize](index: GeneralPurposeRegister with SIBIndexRegister with DoubleQuadSize, base: GeneralPurposeRegister with SIBBaseRegister with DoubleQuadSize, scale: Int, segment: SegmentRegister): SIBMemoryLocation with Size =
          implicitly[SIBForSize[Size]].instance(index, base, None, scale, segment)

        def apply[Size <: ValueSize : SIBForSize](index: GeneralPurposeRegister with SIBIndexRegister with DoubleQuadSize, base: GeneralPurposeRegister with SIBBaseRegister with DoubleQuadSize, displacement: ImmediateValue[_] with ByteWordDoubleSize, scale: Int, segment: SegmentRegister): SIBMemoryLocation with Size =
          implicitly[SIBForSize[Size]].instance(index, base, Some(displacement), scale, segment)
      }

    }

  }


}