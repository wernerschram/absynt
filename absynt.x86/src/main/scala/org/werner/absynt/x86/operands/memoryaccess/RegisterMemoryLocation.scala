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
import org.werner.absynt.x86.operands.ImmediateValue.ValueToByteImmediate
import org.werner.absynt.x86.operands._
import org.werner.absynt.x86.operations.{AddressOperandInfo, AddressSizePrefixRequirement}

import scala.language.implicitConversions

sealed class RegisterMemoryLocation(val reference: RegisterReference, displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)
                                   (implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement)
  extends IndirectMemoryLocation(reference.indexCode,
     if reference.onlyWithDisplacement then
      Some(displacement.getOrElse(byteImmediate(0.toByte)))
    else
      displacement, segment)
    with ModRMEncodableOperand {
  self: ValueSize =>

  override val defaultSegment: SegmentRegister = reference.defaultSegment

  override def addressOperands(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): Set[AddressOperandInfo] = reference match {
    case bi: BaseIndexReference => Set(AddressOperandInfo.rmBase(bi.base), AddressOperandInfo.rmIndex(bi.index, segmentOverride))
    case o: IndexRegister => Set(AddressOperandInfo.rmIndex(o.asInstanceOf[GeneralPurposeRegister & IndexRegister & ValueSize], segmentOverride))
  }

  override def toString: String = s"$sizeName PTR $segmentPrefix[$reference$displacementString]"

  private def displacementString = displacement match {
    case None => ""
    case Some(d) => s"+${d.encodedValue.decimalString}"
  }

  val actualDisplacement: Seq[Byte] =
    if reference.onlyWithDisplacement then
      displacement.map(_.encodedValue).getOrElse(Seq(0.toByte))
    else
      displacement.toSeq.flatMap(_.encodedValue)

  override def getExtendedBytes(rValue: Byte): Seq[Byte] = super.getExtendedBytes(rValue) ++ actualDisplacement
}

class DestinationReference(override val reference: RegisterReference & DestinationIndex, displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement)
  extends RegisterMemoryLocation(reference, displacement, segment) {
  self: ValueSize =>
}

class SourceReference(override val reference: RegisterReference & SourceIndex, displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement)
  extends RegisterMemoryLocation(reference, displacement, segment) {
  self: ValueSize =>
}

object RegisterMemoryLocation {

  abstract class RMForSize[Size <: ValueSize] {
    def instance(reference: RegisterReference, displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement): RegisterMemoryLocation & Size

    def DestinationReference(reference: RegisterReference & DestinationIndex, displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement): DestinationReference & Size

    def SourceReference(reference: RegisterReference & SourceIndex, displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement): SourceReference & Size
  }

  trait I8086Implicits {
    implicit def RMforByteSize: RMForSize[ByteSize] = new RMForSize[ByteSize] {
      override def instance(reference: RegisterReference, displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement): RegisterMemoryLocation & ByteSize =
        new RegisterMemoryLocation(reference, displacement, segment) with ByteSize

      override def DestinationReference(reference: RegisterReference & DestinationIndex, displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement): DestinationReference & ByteSize =
        new DestinationReference(reference, displacement, segment) with ByteSize

      override def SourceReference(reference: RegisterReference & SourceIndex, displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement): SourceReference & ByteSize =
        new SourceReference(reference, displacement, segment) with ByteSize
    }

    implicit def RMforWordSize: RMForSize[WordSize] = new RMForSize[WordSize] {
      override def instance(reference: RegisterReference, displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement): RegisterMemoryLocation & WordSize =
        new RegisterMemoryLocation(reference, displacement, segment) with WordSize

      override def DestinationReference(reference: RegisterReference & DestinationIndex, displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement): DestinationReference & WordSize =
        new DestinationReference(reference, displacement, segment) with WordSize

      override def SourceReference(reference: RegisterReference & SourceIndex, displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement): SourceReference & WordSize =
        new SourceReference(reference, displacement, segment) with WordSize
    }
  }


  trait I386Implicits {
    implicit def RMforDoubleWordSize: RMForSize[DoubleWordSize] = new RMForSize[DoubleWordSize] {
      override def instance(reference: RegisterReference, displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement): RegisterMemoryLocation & DoubleWordSize =
        new RegisterMemoryLocation(reference, displacement, segment) with DoubleWordSize

      override def DestinationReference(reference: RegisterReference & DestinationIndex, displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement): DestinationReference & DoubleWordSize =
        new DestinationReference(reference, displacement, segment) with DoubleWordSize

      override def SourceReference(reference: RegisterReference & SourceIndex, displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement): SourceReference & DoubleWordSize =
        new SourceReference(reference, displacement, segment) with DoubleWordSize
    }
  }

  trait X64Implicits {
    implicit def RMforQuadWordSize: RMForSize[QuadWordSize] = new RMForSize[QuadWordSize] {
      override def instance(reference: RegisterReference, displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement): RegisterMemoryLocation & QuadWordSize =
        new RegisterMemoryLocation(reference, displacement, segment) with QuadWordSize

      override def DestinationReference(reference: RegisterReference & DestinationIndex, displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement): DestinationReference & QuadWordSize =
        new DestinationReference(reference, displacement, segment) with QuadWordSize

      override def SourceReference(reference: RegisterReference & SourceIndex, displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement): SourceReference & QuadWordSize =
        new SourceReference(reference, displacement, segment) with QuadWordSize
    }
  }

  trait Operations {

    object RegisterMemoryLocation {
      def apply[Size <: ValueSize : RMForSize](index: RegisterReference)(implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement): RegisterMemoryLocation & Size =
        implicitly[RMForSize[Size]].instance(index, None, index.defaultSegment)

      def apply[Size <: ValueSize : RMForSize](index: RegisterReference, displacement: ImmediateValue[?] & ByteWordDoubleSize)(implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement): RegisterMemoryLocation & Size =
        implicitly[RMForSize[Size]].instance(index, Some(displacement), index.defaultSegment)

      object withSegmentOverride {
        def apply[Size <: ValueSize : RMForSize](index: RegisterReference, segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement): RegisterMemoryLocation & Size =
          implicitly[RMForSize[Size]].instance(index, None, segment)

        def apply[Size <: ValueSize : RMForSize](index: RegisterReference, displacement: ImmediateValue[?] & ByteWordDoubleSize, segment: SegmentRegister)(implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement): RegisterMemoryLocation & Size =
          implicitly[RMForSize[Size]].instance(index, Some(displacement), segment)
      }

    }

    object DestinationReference {
      implicit def apply[Size <: ValueSize : RMForSize](index: DestinationIndex & IndexRegister)(implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement): DestinationReference & Size =
        implicitly[RMForSize[Size]].DestinationReference(index, None, index.defaultSegment)
    }

    object SourceReference {
      implicit def apply[Size <: ValueSize : RMForSize](index: SourceIndex & IndexRegister)(implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement): SourceReference & Size =
        implicitly[RMForSize[Size]].SourceReference(index, None, index.defaultSegment)
    }


  }

}
