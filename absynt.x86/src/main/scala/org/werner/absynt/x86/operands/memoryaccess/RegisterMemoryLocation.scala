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

import org.werner.absynt.x86.operands._
import MemoryLocation.BaseIndexReference
import org.werner.absynt.x86.operands.registers._
import org.werner.absynt.x86.operations.{AddressOperandInfo, AddressSizePrefixRequirement}

class RegisterMemoryLocation[Size <: WordDoubleQuadSize](
  val reference: BaseIndexReference[
    GeneralPurposeRegister with BasePointerRegister with Size,
    GeneralPurposeRegister with IndexRegister with Size,
    Size,
  ]
) extends MemoryLocation(
      if (
        reference.displacement == 0 && !reference.index.contains(BasePointer.Real) && !reference.index.contains(
          BasePointer.Protected
        ) && !reference.index.contains(BasePointer.Long)
      )
        None
      else if (reference.displacement.toByte == reference.displacement)
        Some(ImmediateValue.byteImmediate(reference.displacement.toByte))
      else if (reference.displacement.toShort == reference.displacement)
        Some(ImmediateValue.wordImmediate(reference.displacement.toShort))
      else
        Some(ImmediateValue.doubleWordImmediate(reference.displacement.toInt)),
      reference.segment,
    )
    with ModRMEncodableOperand {
  self: ValueSize =>

  override val registerOrMemoryModeCode: Byte = (reference.base, reference.index) match {
    case (Some(_: BaseRegister with WordRegister), Some(_: SourceIndex with WordRegister))      => 0
    case (Some(_: BaseRegister with WordRegister), Some(_: DestinationIndex with WordRegister)) => 1
    case (Some(_: BasePointer with WordRegister), Some(_: SourceIndex with WordRegister))       => 2
    case (Some(_: BasePointer with WordRegister), Some(_: DestinationIndex with WordRegister))  => 3
    case (_, Some(index))                                                                       => index.indexCode
    case (_, None)                                                                              => ???
  }

  override val modValue: Byte = {
    (reference.index, displacement) match {
      case (None, Some(_))        => 0x00
      case (_, None)              => 0x00
      case (_, Some(_: ByteSize)) => 0x01
      case (_, _)                 => 0x02
    }
  }

  override val defaultSegment: SegmentRegister =
    reference.base.map(_.defaultSegment).getOrElse(reference.index.map(_.defaultSegment).getOrElse(Segment.Data))

  override def addressOperands(implicit
    addressSizePrefixRequirement: AddressSizePrefixRequirement
  ): Set[AddressOperandInfo] =
    (reference.base.map(AddressOperandInfo.rmBase) ++
      reference.index.map(i => AddressOperandInfo.rmIndex(i, segmentOverride))).toSet

  override def toString: String = s"$sizeName PTR $segmentPrefix[$reference]"

  override def getExtendedBytes(rValue: Byte): Seq[Byte] =
    super.getExtendedBytes(rValue) ++ displacement.map(_.encodedValue).getOrElse(Seq.empty)
}

class DestinationReference[Size <: WordDoubleQuadSize](
  reference: BaseIndexReference[
    GeneralPurposeRegister with BasePointerRegister with Size,
    DestinationIndex with IndexRegister with Size,
    Size,
  ]
) extends RegisterMemoryLocation[Size](reference) {
  self: ValueSize =>
}

class SourceReference[Size <: WordDoubleQuadSize](
  reference: BaseIndexReference[
    Nothing,
    SourceIndex with IndexRegister with Size,
    Size,
  ]
) extends RegisterMemoryLocation(reference) {
  self: ValueSize =>
}

object RegisterMemoryLocation {

  abstract class RMForSize[Size <: ValueSize] {

    def instance[AddressSize <: WordDoubleQuadSize](
      reference: BaseIndexReference[
        GeneralPurposeRegister with BasePointerRegister with AddressSize,
        GeneralPurposeRegister with IndexRegister with AddressSize,
        AddressSize,
      ]
    ): RegisterMemoryLocation[AddressSize] with Size

    def destinationReference[AddressSize <: WordDoubleQuadSize](
      reference: BaseIndexReference[
        Nothing,
        DestinationIndex with IndexRegister with AddressSize,
        AddressSize,
      ]
    ): DestinationReference[AddressSize] with Size

    def sourceReference[AddressSize <: WordDoubleQuadSize](
      reference: BaseIndexReference[
        Nothing,
        SourceIndex with IndexRegister with AddressSize,
        AddressSize,
      ]
    ): SourceReference[AddressSize] with Size
  }

  trait I8086Implicits {

    implicit def RMforByteSize: RMForSize[ByteSize] =
      new RMForSize[ByteSize] {

        override def instance[AddressSize <: WordDoubleQuadSize](
          reference: BaseIndexReference[
            GeneralPurposeRegister with BasePointerRegister with AddressSize,
            GeneralPurposeRegister with IndexRegister with AddressSize,
            AddressSize,
          ]
        ): RegisterMemoryLocation[AddressSize] with ByteSize =
          new RegisterMemoryLocation[AddressSize](reference) with ByteSize

        override def destinationReference[AddressSize <: WordDoubleQuadSize](
          reference: BaseIndexReference[
            Nothing,
            DestinationIndex with IndexRegister with AddressSize,
            AddressSize,
          ]
        ): DestinationReference[AddressSize] with ByteSize =
          new DestinationReference[AddressSize](reference) with ByteSize

        override def sourceReference[AddressSize <: WordDoubleQuadSize](
          reference: BaseIndexReference[
            Nothing,
            SourceIndex with IndexRegister with AddressSize,
            AddressSize,
          ]
        ): SourceReference[AddressSize] with ByteSize =
          new SourceReference[AddressSize](reference) with ByteSize
      }

    implicit def RMforWordSize: RMForSize[WordSize] =
      new RMForSize[WordSize] {

        override def instance[AddressSize <: WordDoubleQuadSize](
          reference: BaseIndexReference[
            GeneralPurposeRegister with BasePointerRegister with AddressSize,
            GeneralPurposeRegister with IndexRegister with AddressSize,
            AddressSize,
          ]
        ): RegisterMemoryLocation[AddressSize] with WordSize =
          new RegisterMemoryLocation[AddressSize](reference) with WordSize

        override def destinationReference[AddressSize <: WordDoubleQuadSize](
          reference: BaseIndexReference[
            Nothing,
            DestinationIndex with IndexRegister with AddressSize,
            AddressSize,
          ]
        ): DestinationReference[AddressSize] with WordSize =
          new DestinationReference[AddressSize](reference) with WordSize

        override def sourceReference[AddressSize <: WordDoubleQuadSize](
          reference: BaseIndexReference[
            Nothing,
            SourceIndex with IndexRegister with AddressSize,
            AddressSize,
          ]
        ): SourceReference[AddressSize] with WordSize =
          new SourceReference[AddressSize](reference) with WordSize
      }
  }

  trait I386Implicits {

    implicit def RMforDoubleWordSize: RMForSize[DoubleWordSize] =
      new RMForSize[DoubleWordSize] {

        override def instance[AddressSize <: WordDoubleQuadSize](
          reference: BaseIndexReference[
            GeneralPurposeRegister with BasePointerRegister with AddressSize,
            GeneralPurposeRegister with IndexRegister with AddressSize,
            AddressSize,
          ]
        ): RegisterMemoryLocation[AddressSize] with DoubleWordSize =
          new RegisterMemoryLocation[AddressSize](reference) with DoubleWordSize

        override def destinationReference[AddressSize <: WordDoubleQuadSize](
          reference: BaseIndexReference[
            Nothing,
            DestinationIndex with IndexRegister with AddressSize,
            AddressSize,
          ]
        ): DestinationReference[AddressSize] with DoubleWordSize =
          new DestinationReference[AddressSize](reference) with DoubleWordSize

        override def sourceReference[AddressSize <: WordDoubleQuadSize](
          reference: BaseIndexReference[
            Nothing,
            SourceIndex with IndexRegister with AddressSize,
            AddressSize,
          ]
        ): SourceReference[AddressSize] with DoubleWordSize =
          new SourceReference[AddressSize](reference) with DoubleWordSize
      }
  }

  trait X64Implicits {

    implicit def RMforQuadWordSize: RMForSize[QuadWordSize] =
      new RMForSize[QuadWordSize] {

        override def instance[AddressSize <: WordDoubleQuadSize](
          reference: BaseIndexReference[
            GeneralPurposeRegister with BasePointerRegister with AddressSize,
            GeneralPurposeRegister with IndexRegister with AddressSize,
            AddressSize,
          ]
        ): RegisterMemoryLocation[AddressSize] with QuadWordSize =
          new RegisterMemoryLocation[AddressSize](reference) with QuadWordSize

        override def destinationReference[AddressSize <: WordDoubleQuadSize](
          reference: BaseIndexReference[
            Nothing,
            DestinationIndex with IndexRegister with AddressSize,
            AddressSize,
          ]
        ): DestinationReference[AddressSize] with QuadWordSize =
          new DestinationReference[AddressSize](reference) with QuadWordSize

        override def sourceReference[AddressSize <: WordDoubleQuadSize](
          reference: BaseIndexReference[
            Nothing,
            SourceIndex with IndexRegister with AddressSize,
            AddressSize,
          ]
        ): SourceReference[AddressSize] with QuadWordSize =
          new SourceReference[AddressSize](reference) with QuadWordSize
      }
  }

}
