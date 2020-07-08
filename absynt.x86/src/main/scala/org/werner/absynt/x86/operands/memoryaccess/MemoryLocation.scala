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

import org.werner.absynt.x86.ArchitectureBounds
import org.werner.absynt.x86.ProcessorMode.I386Bounds
import org.werner.absynt.x86.operands._
import org.werner.absynt.x86.operands.memoryaccess.MemoryAddress.MemoryAddressForSize
import org.werner.absynt.x86.operands.memoryaccess.RegisterMemoryLocation.RMForSize
import org.werner.absynt.x86.operands.memoryaccess.SIBMemoryLocation.SIBForSize
import org.werner.absynt.x86.operands.registers._
import org.werner.absynt.x86.operations.{AddressOperandInfo, AddressSizePrefixRequirement}

import scala.language.implicitConversions

abstract class MemoryLocation(val displacement: Option[ImmediateValue[_]], val segment: SegmentRegister)
  extends ModRMEncodableOperand {

  def addressOperands(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): Set[AddressOperandInfo]

  def defaultSegment: SegmentRegister
  lazy val segmentOverride: Option[SegmentRegister] = if (segment == defaultSegment) None else Some(segment)

  def segmentPrefix: String = segmentOverride match {
    case Some(segmentRegister) => s"$segmentRegister:"
    case None                  => ""
  }
}

object MemoryLocation {

  sealed case class BaseReference[
    +BaseReg <: GeneralPurposeRegister with BasePointerRegister with IndexRegister with Size,
    Size <: WordDoubleQuadSize,
  ](
    baseRegister: BaseReg,
    segment: SegmentRegister,
  ) {

    final def +[
      IndexReg <: GeneralPurposeRegister with CombinableIndexRegister with Size
    ](index: BaseIndexReference[Nothing, IndexReg, Size]): BaseIndexReference[BaseReg, IndexReg, Size] =
      new BaseIndexReference[BaseReg, IndexReg, Size](
        Some(baseRegister),
        index.index,
        segment,
        index.displacement,
        index.scale,
      )

    final def +(displacement: Int): BaseIndexReference[Nothing, BaseReg, Size] =
      BaseIndexReference[Nothing, BaseReg, Size](None, baseRegister, segment, displacement)
  }

  sealed case class BaseIndexReference[
    +BaseReg <: GeneralPurposeRegister with BasePointerRegister with Size,
    +IndexReg <: GeneralPurposeRegister with IndexRegister with Size,
    +Size <: WordDoubleQuadSize,
  ](
    base: Option[BaseReg],
    index: Option[IndexReg],
    segment: SegmentRegister,
    displacement: Long,
    scale: Int,
  ) {

    final val shouldUseSIB: Boolean = base.exists(_.isInstanceOf[DoubleQuadSize]) || scale > 1

    final def +(displacement: Int) =
      new BaseIndexReference[BaseReg, IndexReg, Size](
        base,
        index,
        segment,
        displacement,
        scale,
      )

    final private def scaleString = if (scale > 1) s"*$scale" else ""

    private def baseIndexString = (base ++ index).mkString("+")

    private def displacementString =
      (displacement, base, index) match {
        case (0, _, _) => ""
        case (d, _, _) => s"+${d.toString}"
      }
    override def toString = s"$baseIndexString$scaleString$displacementString"
  }

  object BaseIndexReference {

    def apply[
      BaseReg <: GeneralPurposeRegister with BasePointerRegister with Size,
      IndexReg <: GeneralPurposeRegister with IndexRegister with Size,
      Size <: WordDoubleQuadSize,
    ](
      base: Option[BaseReg],
      index: IndexReg,
      segment: SegmentRegister,
      displacement: Int = 0,
      scale: Int = 1,
    ): BaseIndexReference[BaseReg, IndexReg, Size] =
      BaseIndexReference[BaseReg, IndexReg, Size](base, Some(index), segment, displacement, scale)
  }

  trait I8086Operations {

    implicit def wordBaseRegisterIsBaseRefrence[
      BaseReg <: GeneralPurposeRegister with IndexRegister with BasePointerRegister with WordSize
    ](index: BaseReg): BaseReference[BaseReg, WordSize] =
      BaseReference[BaseReg, WordSize](index, index.defaultSegment)

    implicit def wordIndexRegisterIsBaseIndexReference[
      IndexReg <: GeneralPurposeRegister with IndexRegister with WordSize
    ](index: IndexReg): BaseIndexReference[Nothing, IndexReg, WordSize] =
      BaseIndexReference[Nothing, IndexReg, WordSize](None, index, index.defaultSegment)

    implicit def immediateValueIsBaseIndexReference(
      immediateValue: Long
    ): BaseIndexReference[Nothing, Nothing, Nothing] =
      new BaseIndexReference[Nothing, Nothing, Nothing](None, None, Segment.Data, immediateValue, 1)

    trait I8086Pointer {

      def word[
        Size <: ValueSize : RMForSize : MemoryAddressForSize
      ](
        baseIndexReference: BaseIndexReference[
          GeneralPurposeRegister with BasePointerRegister with WordSize,
          GeneralPurposeRegister with IndexRegister with WordSize,
          WordSize,
        ]
      ): MemoryLocation with Size = baseIndexReference match {
        case BaseIndexReference(None, None, segment, displacement, _) =>
          implicitly[MemoryAddressForSize[Size]].instance(displacement, segment)
        case reference: BaseIndexReference[
          GeneralPurposeRegister with BasePointerRegister with DoubleWordSize,
          GeneralPurposeRegister with IndexRegister with DoubleWordSize,
          DoubleWordSize,
        ] =>
          implicitly[RMForSize[Size]].instance[WordSize](reference)
      }
    }

    trait I8086DestinationReference {

      def word[Size <: ValueSize : RMForSize](
        index: BaseIndexReference[
          Nothing,
          DestinationIndex with IndexRegister with WordSize,
          WordSize,
        ]
      ): DestinationReference[WordSize] with Size = {
        implicitly[RMForSize[Size]].destinationReference(index)
      }
    }

    trait I8086SourceReference {

      def word[Size <: ValueSize : RMForSize](
        index: BaseIndexReference[
          Nothing,
          SourceIndex with IndexRegister with WordSize,
          WordSize,
        ]
      ): SourceReference[WordSize] with Size = {
        implicitly[RMForSize[Size]].sourceReference(index)
      }
    }
  }

  trait LegacyOperations extends I8086Operations {

    object Pointer extends I8086Pointer {

      object Destination extends I8086DestinationReference

      object Source extends I8086SourceReference
    }
  }

  trait I386Operations extends I8086Operations {
    self: ArchitectureBounds with MemoryAddress.I386Implicits =>

    trait NoFactor[
      +BaseReg <: GeneralPurposeRegister with BasePointerRegister with Size,
      +IndexReg <: GeneralPurposeRegister with IndexRegister with Size,
      Size <: DoubleQuadSize
    ] {
      self: BaseIndexReference[BaseReg, IndexReg, Size] =>

      def *(newScale: Int): BaseIndexReference[BaseReg, IndexReg, Size] = {
        BaseIndexReference[BaseReg, IndexReg, Size](base, index, segment, displacement, newScale)
      }
    }

    implicit def doubleWordBaseRegisterIsBaseRefrence[
      BaseReg <: GeneralPurposeRegister with IndexRegister with BasePointerRegister with DoubleWordSize
    ](index: BaseReg): BaseReference[BaseReg, DoubleWordSize] =
      BaseReference[BaseReg, DoubleWordSize](index, index.defaultSegment)

    implicit def doubleWordIndexRegisterIsBaseIndexReference[
      IndexReg <: GeneralPurposeRegister with IndexRegister with DoubleWordSize
    ](
      index: IndexReg
    ): BaseIndexReference[Nothing, IndexReg, DoubleWordSize] with NoFactor[Nothing, IndexReg, DoubleWordSize] =
      new BaseIndexReference[Nothing, IndexReg, DoubleWordSize](None, Some(index), index.defaultSegment, 0, 1)
        with NoFactor[Nothing, IndexReg, DoubleWordSize]

    trait I386Pointer extends I8086Pointer {

      def doubleWord[
        Size <: ValueSize : RMForSize : SIBForSize : MemoryAddressForSize
      ](
        baseIndexReference: BaseIndexReference[
          GeneralPurposeRegister with BasePointerRegister with DoubleWordSize,
          GeneralPurposeRegister with IndexRegister with DoubleWordSize,
          DoubleWordSize,
        ]
      ): MemoryLocation with Size =
        baseIndexReference match {
          case BaseIndexReference(None, None, segment, displacement, _) =>
            implicitly[MemoryAddressForSize[Size]].instance(displacement, segment)
          case reference: BaseIndexReference[
                GeneralPurposeRegister with BasePointerRegister with DoubleWordSize,
                GeneralPurposeRegister with IndexRegister with DoubleWordSize,
                DoubleWordSize,
              ] if !reference.shouldUseSIB =>
            implicitly[RMForSize[Size]].instance[DoubleWordSize](reference)
          case reference: BaseIndexReference[
                GeneralPurposeRegister with BasePointerRegister with DoubleWordSize,
                GeneralPurposeRegister with IndexRegister with DoubleWordSize,
                DoubleWordSize,
              ] =>
            implicitly[SIBForSize[Size]].instance(reference)
        }
    }

    trait I386DestinationReference extends I8086DestinationReference {

      def doubleWord[Size <: ValueSize : RMForSize](
        index: BaseIndexReference[
          Nothing,
          DestinationIndex with IndexRegister with DoubleWordSize,
          DoubleWordSize,
        ]
      ): DestinationReference[DoubleWordSize] with Size = {
        implicitly[RMForSize[Size]].destinationReference(index)
      }
    }

    trait I386SourceReference extends I8086SourceReference {

      def doubleWord[Size <: ValueSize : RMForSize](
        index: BaseIndexReference[
          Nothing,
          SourceIndex with IndexRegister with DoubleWordSize,
          DoubleWordSize,
        ]
      ): SourceReference[DoubleWordSize] with Size = {
        implicitly[RMForSize[Size]].sourceReference(index)
      }
    }

  }

  trait RealOperations extends I386Operations {
    self: I386Bounds with MemoryAddress.I386Implicits =>

    object Pointer extends I386Pointer {

      object Destination extends I386DestinationReference

      object Source extends I386SourceReference
    }
  }

  trait X64Operations extends I386Operations {
    self: ArchitectureBounds with MemoryAddress.X64Implicits =>

    implicit def quadWordBaseRegisterIsBaseRefrence[
      BaseReg <: GeneralPurposeRegister with IndexRegister with BasePointerRegister with QuadWordSize
    ](index: BaseReg): BaseReference[BaseReg, QuadWordSize] =
      BaseReference[BaseReg, QuadWordSize](index, index.defaultSegment)

    implicit def quadWordIndexRegisterIsBaseIndexReference[
      IndexReg <: GeneralPurposeRegister with IndexRegister with QuadWordSize
    ](
      index: IndexReg
    ): BaseIndexReference[Nothing, IndexReg, QuadWordSize] with NoFactor[Nothing, IndexReg, QuadWordSize] =
      new BaseIndexReference[Nothing, IndexReg, QuadWordSize](None, Some(index), index.defaultSegment, 0, 1)
        with NoFactor[Nothing, IndexReg, QuadWordSize]

    object Pointer extends I386Pointer {

      def quadWord[
        Size <: ValueSize : RMForSize : SIBForSize : MemoryAddressForSize
      ](
        baseIndexReference: BaseIndexReference[
          GeneralPurposeRegister with BasePointerRegister with QuadWordSize,
          GeneralPurposeRegister with IndexRegister with QuadWordSize,
          QuadWordSize,
        ]
      ): MemoryLocation with Size =
        baseIndexReference match {
          case BaseIndexReference(None, None, segment, displacement, _) =>
            implicitly[MemoryAddressForSize[Size]].instance(displacement, segment)
          case reference: BaseIndexReference[
                GeneralPurposeRegister with BasePointerRegister with QuadWordSize,
                GeneralPurposeRegister with IndexRegister with QuadWordSize,
                QuadWordSize,
              ] if !reference.shouldUseSIB =>
            implicitly[RMForSize[Size]].instance[QuadWordSize](reference)
          case reference: BaseIndexReference[
                GeneralPurposeRegister with BasePointerRegister with QuadWordSize,
                GeneralPurposeRegister with IndexRegister with QuadWordSize,
                QuadWordSize,
              ] =>
            implicitly[SIBForSize[Size]].instance(reference)
        }

      object Destination extends I386DestinationReference {

        def quadWord[Size <: ValueSize : RMForSize](
          index: BaseIndexReference[
            Nothing,
            DestinationIndex with IndexRegister with QuadWordSize,
            QuadWordSize,
          ]
        ): DestinationReference[QuadWordSize] with Size = {
          implicitly[RMForSize[Size]].destinationReference(index)
        }
      }

      object Source extends I386SourceReference {

        def quadWord[Size <: ValueSize : RMForSize](
          index: BaseIndexReference[
            Nothing,
            SourceIndex with IndexRegister with QuadWordSize,
            QuadWordSize,
          ]
        ): SourceReference[QuadWordSize] with Size = {
          implicitly[RMForSize[Size]].sourceReference(index)
        }
      }
    }
  }
}
