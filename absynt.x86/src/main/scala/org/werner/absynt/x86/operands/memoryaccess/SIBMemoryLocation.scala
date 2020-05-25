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
import org.werner.absynt.x86.operands.registers.{BasePointer, GeneralPurposeRegister, LongSIBBaseRegister, LongSIBIndexRegister, ProtectedSIBBaseRegister, ProtectedSIBIndexRegister, SIBBaseRegister, SIBIndexRegister, Segment, SegmentRegister}
import org.werner.absynt.x86.operands.{ModRMEncodableOperand, _}
import org.werner.absynt.x86.operations.{AddressOperandInfo, AddressSizePrefixRequirement}

sealed abstract class SIBMemoryLocation(
    val index: Option[GeneralPurposeRegister with SIBIndexRegister with DoubleQuadSize],
    val base: Option[GeneralPurposeRegister with SIBBaseRegister with DoubleQuadSize],
    displacementValue: Option[ImmediateValue[_] with ByteWordDoubleSize] = None, val scale: Int, segment: SegmentRegister)
  extends MemoryLocation(
    if (base.contains(BasePointer.Protected) || base.contains(BasePointer.Long)) displacementValue orElse Some(ImmediateValue.byteImmediate(0)) else displacementValue,
    segment
  ) with ModRMEncodableOperand {

  self: ValueSize =>

  val modValue: Byte = {
    (displacement, base, index) match {
      case (None, _, _) => 0x00
      case (Some(_:ByteSize),_ ,_) => 0x01
      case (Some(_:DoubleWordSize),None, None) => 0x00
      case (Some(_:DoubleWordSize),_, _) => 0x02
    }
  }

  override val registerOrMemoryModeCode: Byte = 0x04.toByte

//  assume(index sizeEquals base)
  assume((1 :: 2 :: 4 :: 8 :: Nil).contains(scale))

  // TODO: simplify: default is always data
  override val defaultSegment: SegmentRegister = index.map(_.defaultSegment).getOrElse(Segment.Data)
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

  private def scaleString = if (scale > 1) s"*$scale" else ""

  private def displacementString = (displacementValue, base, index) match {
    case (None, _, _) => ""
    case (Some(d), None, None) => d.encodedValue.decimalString
    case (Some(d), _, _) => s"+${d.encodedValue.decimalString}"
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

  trait I386Implicits {

    abstract class ProtectedSIBForSize[Size <: ValueSize]() {
      def instance(sib: ProtectedSIB): SIBMemoryLocation with Size
    }

    implicit def ProtectedSIBforByteSize: ProtectedSIBForSize[ByteSize] =
      (sib: ProtectedSIB) => new SIBMemoryLocation(sib.index, sib.base, sib.displacement, sib.scale, sib.segment) with ByteSize

    implicit def ProtectedSIBforWordSize: ProtectedSIBForSize[WordSize] =
      (sib: ProtectedSIB) => new SIBMemoryLocation(sib.index, sib.base, sib.displacement, sib.scale, sib.segment) with WordSize

    implicit def ProtectedSIBforDoubleWordSize: ProtectedSIBForSize[DoubleWordSize] =
      (sib: ProtectedSIB) => new SIBMemoryLocation(sib.index, sib.base, sib.displacement, sib.scale, sib.segment) with DoubleWordSize
  }

  trait I386Operations extends I386Implicits {
    object SIBMemoryLocation {
      def apply[Size <: ValueSize : ProtectedSIBForSize](sib: ProtectedSIB): SIBMemoryLocation with Size =
        implicitly[ProtectedSIBForSize[Size]].instance(sib)
    }
  }

  trait LongOperations extends I386Implicits {
    abstract class LongSIBForSize[Size<:ValueSize]() {
      def instance(sib: LongSIB): SIBMemoryLocation with Size
    }

    implicit def ProtectedSIBforQuadWordSize: ProtectedSIBForSize[QuadWordSize] =
      (sib: ProtectedSIB) => new SIBMemoryLocation(sib.index, sib.base, sib.displacement, sib.scale, sib.segment) with QuadWordSize

    implicit def LongSIBforByteSize: LongSIBForSize[ByteSize] =
      (sib: LongSIB) => new SIBMemoryLocation(sib.index, sib.base, sib.displacement, sib.scale, sib.segment) with ByteSize

    implicit def LongSIBforWordSize: LongSIBForSize[WordSize] =
      (sib: LongSIB) => new SIBMemoryLocation(sib.index, sib.base, sib.displacement, sib.scale, sib.segment) with WordSize

    implicit def LongSIBforDoubleWordSize: LongSIBForSize[DoubleWordSize] =
      (sib: LongSIB) => new SIBMemoryLocation(sib.index, sib.base, sib.displacement, sib.scale, sib.segment) with DoubleWordSize

    implicit def LongSIBforQuadWordSize: LongSIBForSize[QuadWordSize] =
      (sib: LongSIB) => new SIBMemoryLocation(sib.index, sib.base, sib.displacement, sib.scale, sib.segment) with QuadWordSize

    object SIBMemoryLocation {
      def apply[Size <: ValueSize : ProtectedSIBForSize](sib: ProtectedSIB): SIBMemoryLocation with Size =
        implicitly[ProtectedSIBForSize[Size]].instance(sib)

      def apply[Size <: ValueSize : LongSIBForSize](sib: LongSIB): SIBMemoryLocation with Size =
        implicitly[LongSIBForSize[Size]].instance(sib)
    }

  }


}