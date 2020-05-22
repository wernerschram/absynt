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
    val index: Option[GeneralPurposeRegister with SIBIndexRegister with DoubleQuadSize],
    val base: Option[GeneralPurposeRegister with SIBBaseRegister with DoubleQuadSize],
    displacement: Option[ImmediateValue[_] with ByteWordDoubleSize] = None, val scale: Int, segment: SegmentRegister)
  extends IndirectMemoryLocation(0x04, displacement, segment) with ModRMEncodableOperand {

  self: ValueSize =>

//  assume(index sizeEquals base)
  assume((1 :: 2 :: 4 :: 8 :: Nil).contains(scale))

  // TODO: simplify: default is always data
  override val defaultSegment: SegmentRegister = index.map(_.defaultSIBSegment).getOrElse(Segment.Data)
  val baseCode: Byte = base.map(_.SIBBaseCode).getOrElse(5.toByte)
  val indexCode: Byte = index.map(_.SIBIndexCode).getOrElse(4.toByte)

  override def addressOperands(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): Set[AddressOperandInfo] =
    (base.map(AddressOperandInfo.SIBBase) ++ index.map(i => AddressOperandInfo.SIBIndex(i, segmentOverride))).toSet

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

  private def baseIndexString = (base ++ index).mkString("+")

  override def toString = s"$sizeName PTR $segmentPrefix[$baseIndexString$scaleString$displacementString]"

  private def scaleString = s"*$scale"

  private def displacementString = displacement match {
    case None => ""
    case Some(d) => s"+${d.encodedValue.decimalString}"
  }
}

trait ProtectedSIB {
  def base: Option[GeneralPurposeRegister with ProtectedSIBBaseRegister with DoubleWordSize]
  def index: Option[GeneralPurposeRegister with ProtectedSIBIndexRegister with DoubleWordSize]
  def scale: Int
  def displacement: Option[ImmediateValue[Int] with DoubleWordSize]
  def segment: SegmentRegister
}

trait LongSIB {
  def base: Option[GeneralPurposeRegister with LongSIBBaseRegister with QuadWordSize]
  def index: Option[GeneralPurposeRegister with LongSIBIndexRegister with QuadWordSize]
  def scale: Int
  def displacement: Option[ImmediateValue[Int] with DoubleWordSize]
  def segment: SegmentRegister
}

object SIBMemoryLocation {

  trait I386Operations {
    abstract class SIBForSize[Size<:ValueSize]() {
      def instance(index: Option[GeneralPurposeRegister with SIBIndexRegister with DoubleWordSize], base: Option[GeneralPurposeRegister with SIBBaseRegister with DoubleWordSize], displacement: Option[ImmediateValue[_] with ByteWordDoubleSize], scale: Int, segmentOverride: SegmentRegister): SIBMemoryLocation with Size
    }

    implicit def SIBforByteSize: SIBForSize[ByteSize] =
      (index: Option[GeneralPurposeRegister with SIBIndexRegister with DoubleWordSize], base: Option[GeneralPurposeRegister with SIBBaseRegister with DoubleWordSize], displacement: Option[ImmediateValue[_] with ByteWordDoubleSize], scale: Int, segmentOverride: SegmentRegister) =>
        new SIBMemoryLocation(index, base, displacement, scale, segmentOverride) with ByteSize

    implicit def SIBforWordSize: SIBForSize[WordSize] =
      (index: Option[GeneralPurposeRegister with SIBIndexRegister with DoubleWordSize], base: Option[GeneralPurposeRegister with SIBBaseRegister with DoubleWordSize], displacement: Option[ImmediateValue[_] with ByteWordDoubleSize], scale: Int, segmentOverride: SegmentRegister) =>
        new SIBMemoryLocation(index, base, displacement, scale, segmentOverride) with WordSize

    implicit def SIBforDoubleWordSize: SIBForSize[DoubleWordSize] =
      (index: Option[GeneralPurposeRegister with SIBIndexRegister with DoubleWordSize], base: Option[GeneralPurposeRegister with SIBBaseRegister with DoubleWordSize], displacement: Option[ImmediateValue[_] with ByteWordDoubleSize], scale: Int, segmentOverride: SegmentRegister) =>
        new SIBMemoryLocation(index, base, displacement, scale, segmentOverride) with DoubleWordSize

    object SIBMemoryLocation {
      def apply[Size <: ValueSize : SIBForSize](sib: ProtectedSIB): SIBMemoryLocation with Size =
        implicitly[SIBForSize[Size]].instance(sib.index, sib.base, sib.displacement, sib.scale, sib.segment)
    }
  }

  trait LongOperations {
    abstract class SIBForSize[Size<:ValueSize]() {
      def instance(index: Option[GeneralPurposeRegister with SIBIndexRegister with DoubleQuadSize], base: Option[GeneralPurposeRegister with SIBBaseRegister with DoubleQuadSize], displacement: Option[ImmediateValue[_] with ByteWordDoubleSize], scale: Int, segmentOverride: SegmentRegister): SIBMemoryLocation with Size
    }

    implicit def SIBforByteSize: SIBForSize[ByteSize] =
      (index: Option[GeneralPurposeRegister with SIBIndexRegister with DoubleQuadSize], base: Option[GeneralPurposeRegister with SIBBaseRegister with DoubleQuadSize], displacement: Option[ImmediateValue[_] with ByteWordDoubleSize], scale: Int, segmentOverride: SegmentRegister) =>
        new SIBMemoryLocation(index, base, displacement, scale, segmentOverride) with ByteSize

    implicit def SIBforWordSize: SIBForSize[WordSize] =
      (index: Option[GeneralPurposeRegister with SIBIndexRegister with DoubleQuadSize], base: Option[GeneralPurposeRegister with SIBBaseRegister with DoubleQuadSize], displacement: Option[ImmediateValue[_] with ByteWordDoubleSize], scale: Int, segmentOverride: SegmentRegister) =>
        new SIBMemoryLocation(index, base, displacement, scale, segmentOverride) with WordSize

    implicit def SIBforDoubleWordSize: SIBForSize[DoubleWordSize] =
      (index: Option[GeneralPurposeRegister with SIBIndexRegister with DoubleQuadSize], base: Option[GeneralPurposeRegister with SIBBaseRegister with DoubleQuadSize], displacement: Option[ImmediateValue[_] with ByteWordDoubleSize], scale: Int, segmentOverride: SegmentRegister) =>
        new SIBMemoryLocation(index, base, displacement, scale, segmentOverride) with DoubleWordSize

    implicit def SIBforQuadWordSize: SIBForSize[QuadWordSize] =
      (index: Option[GeneralPurposeRegister with SIBIndexRegister with DoubleQuadSize], base: Option[GeneralPurposeRegister with SIBBaseRegister with DoubleQuadSize], displacement: Option[ImmediateValue[_] with ByteWordDoubleSize], scale: Int, segmentOverride: SegmentRegister) =>
        new SIBMemoryLocation(index, base, displacement, scale, segmentOverride) with QuadWordSize

    object SIBMemoryLocation {
      def apply[Size <: ValueSize : SIBForSize](sib: LongSIB): SIBMemoryLocation with Size =
        implicitly[SIBForSize[Size]].instance(sib.index, sib.base, sib.displacement, sib.scale, sib.segment)
    }

  }


}