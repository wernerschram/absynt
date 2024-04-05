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
                                   (using ValueToByteImmediate, AddressSizePrefixRequirement)
  extends IndirectMemoryLocation(reference.indexCode,
     if reference.onlyWithDisplacement then
      Some(displacement.getOrElse(0.toByte))
    else
      displacement, segment)
    with ModRMEncodableOperand {
  self: ValueSize =>

  override val defaultSegment: SegmentRegister = reference.defaultSegment

  override def addressOperands(using addressSizePrefixRequirement: AddressSizePrefixRequirement): Set[AddressOperandInfo] = reference match {
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

class DestinationReference(override val reference: RegisterReference & DestinationIndex, displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(using ValueToByteImmediate, AddressSizePrefixRequirement)
  extends RegisterMemoryLocation(reference, displacement, segment) {
  self: ValueSize =>
}

class SourceReference(override val reference: RegisterReference & SourceIndex, displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(using ValueToByteImmediate, AddressSizePrefixRequirement)
  extends RegisterMemoryLocation(reference, displacement, segment) {
  self: ValueSize =>
}

object RegisterMemoryLocation {
  trait RMForSize[Size <: ValueSize]:
    extension(reference: RegisterReference) def instance(displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(using ValueToByteImmediate, AddressSizePrefixRequirement): RegisterMemoryLocation & Size

    extension(reference: RegisterReference & DestinationIndex) def destinationReference(displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(using ValueToByteImmediate, AddressSizePrefixRequirement): DestinationReference & Size

    extension(reference: RegisterReference & SourceIndex) def sourceReference(displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(using ValueToByteImmediate, AddressSizePrefixRequirement): SourceReference & Size


  trait I8086Implicits {
    given RMForSize[ByteSize] with {
      extension (reference: RegisterReference) override def instance(displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(using ValueToByteImmediate, AddressSizePrefixRequirement): RegisterMemoryLocation & ByteSize =
        new RegisterMemoryLocation(reference, displacement, segment) with ByteSize

      extension (reference: RegisterReference & DestinationIndex) override def destinationReference(displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(using ValueToByteImmediate, AddressSizePrefixRequirement): DestinationReference & ByteSize =
        new DestinationReference(reference, displacement, segment) with ByteSize

      extension (reference: RegisterReference & SourceIndex) override def sourceReference(displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(using ValueToByteImmediate, AddressSizePrefixRequirement): SourceReference & ByteSize =
        new SourceReference(reference, displacement, segment) with ByteSize
    }

    given RMForSize[WordSize] with {
      extension (reference: RegisterReference) override def instance(displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(using ValueToByteImmediate, AddressSizePrefixRequirement): RegisterMemoryLocation & WordSize =
        new RegisterMemoryLocation(reference, displacement, segment) with WordSize

      extension (reference: RegisterReference & DestinationIndex) override def destinationReference(displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(using ValueToByteImmediate, AddressSizePrefixRequirement): DestinationReference & WordSize =
        new DestinationReference(reference, displacement, segment) with WordSize

      extension (reference: RegisterReference & SourceIndex) override def sourceReference(displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(using ValueToByteImmediate, AddressSizePrefixRequirement): SourceReference & WordSize =
        new SourceReference(reference, displacement, segment) with WordSize
    }
  }


  trait I386Implicits {
    given RMForSize[DoubleWordSize] with {
      extension(reference: RegisterReference) override def instance(displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(using ValueToByteImmediate, AddressSizePrefixRequirement): RegisterMemoryLocation & DoubleWordSize =
        new RegisterMemoryLocation(reference, displacement, segment) with DoubleWordSize

      extension (reference: RegisterReference & DestinationIndex) override def destinationReference(displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(using ValueToByteImmediate, AddressSizePrefixRequirement): DestinationReference & DoubleWordSize =
        new DestinationReference(reference, displacement, segment) with DoubleWordSize

      extension (reference: RegisterReference & SourceIndex) override def sourceReference(displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(using ValueToByteImmediate, AddressSizePrefixRequirement): SourceReference & DoubleWordSize =
        new SourceReference(reference, displacement, segment) with DoubleWordSize
    }
  }

  trait X64Implicits {
    given RMForSize[QuadWordSize] with {
      extension(reference: RegisterReference) override def instance(displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(using ValueToByteImmediate, AddressSizePrefixRequirement): RegisterMemoryLocation & QuadWordSize =
        new RegisterMemoryLocation(reference, displacement, segment) with QuadWordSize

      extension (reference: RegisterReference & DestinationIndex) override def destinationReference(displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(using ValueToByteImmediate, AddressSizePrefixRequirement): DestinationReference & QuadWordSize =
        new DestinationReference(reference, displacement, segment) with QuadWordSize

      extension (reference: RegisterReference & SourceIndex) override def sourceReference(displacement: Option[ImmediateValue[?] & ByteWordDoubleSize], segment: SegmentRegister)(using ValueToByteImmediate, AddressSizePrefixRequirement): SourceReference & QuadWordSize =
        new SourceReference(reference, displacement, segment) with QuadWordSize
    }
  }

  trait Operations {

    object RegisterMemoryLocation {
      def apply[Size <: ValueSize : RMForSize](index: RegisterReference)(using ValueToByteImmediate, AddressSizePrefixRequirement): RegisterMemoryLocation & Size =
        index.instance(None, index.defaultSegment)

      def apply[Size <: ValueSize : RMForSize](index: RegisterReference, displacement: ImmediateValue[?] & ByteWordDoubleSize)(using ValueToByteImmediate, AddressSizePrefixRequirement): RegisterMemoryLocation & Size =
        index.instance(Some(displacement), index.defaultSegment)

      object withSegmentOverride {
        def apply[Size <: ValueSize : RMForSize](index: RegisterReference, segment: SegmentRegister)(using ValueToByteImmediate, AddressSizePrefixRequirement): RegisterMemoryLocation & Size =
          index.instance(None, segment)

        def apply[Size <: ValueSize : RMForSize](index: RegisterReference, displacement: ImmediateValue[?] & ByteWordDoubleSize, segment: SegmentRegister)(using ValueToByteImmediate, AddressSizePrefixRequirement): RegisterMemoryLocation & Size =
          index.instance(Some(displacement), segment)
      }
    }

    object DestinationReference {
      def apply[Size <: ValueSize : RMForSize](index: DestinationIndex & IndexRegister)(using ValueToByteImmediate, AddressSizePrefixRequirement): DestinationReference & Size =
        index.destinationReference(None, index.defaultSegment)
    }

    object SourceReference {
      def apply[Size <: ValueSize : RMForSize](index: SourceIndex & IndexRegister)(using ValueToByteImmediate, AddressSizePrefixRequirement): SourceReference & Size =
        index.sourceReference(None, index.defaultSegment)
    }
  }
}
