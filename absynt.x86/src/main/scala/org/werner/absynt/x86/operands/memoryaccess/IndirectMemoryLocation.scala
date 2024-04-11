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

import org.werner.absynt.x86.operands.ImmediateValue.{ValueToByteImmediate, byteImmediate, doubleWordImmediate, wordImmediate}
import org.werner.absynt.x86.operands.Register.{I386GenericRegisters, I8086SpecificRegisters}
import org.werner.absynt.x86.operands.*
import org.werner.absynt.x86.operations.{AddressOperandInfo, AddressSizePrefixRequirement}

import scala.annotation.targetName

abstract class IndirectMemoryLocationOld(val registerOrMemoryModeCode: Byte, displacement: Option[ImmediateValue[?] & ByteWordDoubleSize] = None,
                                         segment: SegmentRegister)
  extends MemoryLocation(displacement, segment) {

  val modValue: Byte = {
    displacement match {
      case None => 0x00
      case Some(d) => if d.isInstanceOf[ByteSize] then 0x01 else 0x02
    }
  }
}

abstract class IndirectMemoryLocation(
  override val registerOrMemoryModeCode: Byte,
  override val modValue: Byte,
  displacement: Option[ImmediateValue[?] & ByteWordDoubleSize] = None,
  sibValue: Option[Byte],
  segment: SegmentRegister,
  val addressOperandSet: Set[AddressOperandInfo],
  override val defaultSegment: SegmentRegister
)
  extends MemoryLocation(displacement, segment) {
  override def addressOperands(using AddressSizePrefixRequirement): Set[AddressOperandInfo] =
    addressOperandSet

  override def getExtendedBytes(rValue: Byte): Seq[Byte] = super.getExtendedBytes(rValue) ++ sibValue.toSeq ++ displacement.toSeq.flatMap(_.encodedValue)
}

object IndirectMemoryLocation {

  class DestinationReference(
    reference: RegisterReference & DestinationIndex & IndexRegister & WordDoubleQuadSize,
    segment: SegmentRegister,
  )(using AddressSizePrefixRequirement) extends IndirectMemoryLocation(
    if reference.isInstanceOf[WordSize] then 6.toByte else 7.toByte,
    0.toByte,
    None,
    None,
    segment,
    Set(AddressOperandInfo.rmIndex(reference, None)),
    reference.defaultSegment
  ) {
    self: ValueSize =>

    override def toString: String = s"$sizeName PTR $segmentPrefix[$reference]"
  }

  class SourceReference(
    reference: RegisterReference & SourceIndex & IndexRegister & WordDoubleQuadSize,
    segment: SegmentRegister,
  )(using AddressSizePrefixRequirement) extends IndirectMemoryLocation(
    if reference.isInstanceOf[WordSize] then 5.toByte else 6.toByte,
    0.toByte,
    None,
    None,
    segment,
    Set(AddressOperandInfo.rmIndex(reference, None)),
    reference.defaultSegment
  ) {
    self: ValueSize =>

    override def toString: String = s"$sizeName PTR $segmentPrefix[$reference]"
  }

  trait IndirectMemoryLocationForSize[Size <: ValueSize](using AddressSizePrefixRequirement):

    def instance(
        referenceBuilder: ReferenceBuilder
               ): IndirectMemoryLocation & Size

    def destination(reference: RegisterReference & DestinationIndex & IndexRegister & WordDoubleQuadSize,
                    segment: SegmentRegister): DestinationReference & Size

    def source(reference: RegisterReference & SourceIndex & IndexRegister & WordDoubleQuadSize,
               segment: SegmentRegister): SourceReference & Size


  trait ReferenceBuilder

  trait I8086Operations {
    self: I8086SpecificRegisters & ImmediateValue.I8086Implicits =>


    class RealIndirectMemoryLocation(
                                      referenceBuilder: RealModeReferenceBuilder
                                    )(using AddressSizePrefixRequirement) extends IndirectMemoryLocation(
      referenceBuilder.registerOrMemoryModeCode,
      referenceBuilder.modValue,
      referenceBuilder.displacementValue,
      None,
      referenceBuilder.segment,
      referenceBuilder.addressOperands,
      referenceBuilder.defaultSegment
    ) {
      self: ValueSize =>
      override def toString: String =
        s"$sizeName PTR ${referenceBuilder.toString}"
    }

    sealed abstract class RealModeReferenceBuilder extends ReferenceBuilder {
      def base: Option[GeneralPurposeRegister & RealModeBaseRegister & WordSize]

      def index: Option[GeneralPurposeRegister & RealModeIndexRegister & WordSize]

      def displacement: Int

      def segment: SegmentRegister

      final def defaultSegment: SegmentRegister =
        base.map(_.defaultSegment).getOrElse(index.map(_.defaultSegment).getOrElse(Segment.Data))

      override def toString: String = {
        val seg = if segment != defaultSegment then s"$segment:" else ""
        val baseIndex = (base, index) match {
          case (Some(base), Some(index)) if displacement == 0 =>
            s"$base+$index"
          case (Some(base), Some(index)) =>
            s"$base+$index+$displacement"
          case (Some(base), None) if displacement == 0 =>
            s"$base"
          case (Some(base), None) =>
            s"$base+$displacement"
          case (None, Some(index)) if displacement == 0 =>
            s"$index"
          case (None, Some(index)) =>
            s"$index+$displacement"
          case (None, None) if displacement == 0 =>
            s""
          case (None, None) =>
            displacement.toString
        }

        s"$seg[$baseIndex]"
      }

      private[I8086Operations] def registerOrMemoryModeCode: Byte = (base, index) match {
        case (Some(BX), Some(SI)) => 0.toByte
        case (Some(BX), Some(DI)) => 1.toByte
        case (Some(BP), Some(SI)) => 2.toByte
        case (Some(BP), Some(DI)) => 3.toByte
        case (None, Some(SI)) => 4.toByte
        case (None, Some(DI)) => 5.toByte
        case (Some(BP), None) => 6.toByte
        case (None, None) => 6.toByte
        case (Some(BX), None) => 7.toByte
        case _ => throw new AssertionError(s"Invalid combination of operands for indirect memory location $base $index")
      }

      private[I8086Operations] def addressOperands(using AddressSizePrefixRequirement) =
        base.map(b => AddressOperandInfo.rmBase(b)).toSet ++
          index.map(i => AddressOperandInfo.rmIndex(i, Option.when(segment != defaultSegment)(segment))).toSet


      private[I8086Operations] def modValue =
        if displacement == 0 then
          if base.contains(BP) && index.isEmpty then 1.toByte else 0.toByte
        else if displacement < 256 then 1.toByte
        else 2.toByte


      private[I8086Operations] def displacementValue =
        if displacement == 0 then
          if base.contains(BP) && index.isEmpty then Some(byteImmediate(0)) else None
        else if displacement < 256 then Some(byteImmediate(displacement.toByte))
        else Some(wordImmediate(displacement.toShort))


    }

    @targetName("asBaseReference")
    given Conversion[GeneralPurposeRegister & RealModeBaseRegister & WordSize, RealBaseReferenceBuilder] =
      baseRegister => RealBaseReferenceBuilder(baseRegister, baseRegister.defaultSegment)

    @targetName("asIndexReference")
    given Conversion[GeneralPurposeRegister & RealModeIndexRegister & WordSize, RealIndexReferenceBuilder] =
      indexRegister => RealIndexReferenceBuilder(indexRegister, indexRegister.defaultSegment)

    given Conversion[Int, RealDisplacementReferenceBuilder] =
      displacement => RealDisplacementReferenceBuilder(None, None, Segment.Data, displacement)

    case class RealBaseReferenceBuilder(
                                     baseRegister: GeneralPurposeRegister & RealModeBaseRegister & WordSize,
                                     override val segment: SegmentRegister
                                   ) extends RealModeReferenceBuilder {
      override def base: Option[GeneralPurposeRegister & RealModeBaseRegister & WordSize] = Some(baseRegister)

      override def index: Option[GeneralPurposeRegister & RealModeIndexRegister & WordSize] = None

      override def displacement: Int = 0

      @targetName("withIndex")
      def +(indexRegister: GeneralPurposeRegister & RealModeIndexRegister & WordSize): RealBaseIndexReferenceBuilder =
        RealBaseIndexReferenceBuilder(baseRegister, indexRegister, segment)

      @targetName("withDisplacement")
      def +(displacementValue: Int): RealDisplacementReferenceBuilder =
        RealDisplacementReferenceBuilder(base, index, segment, displacementValue)
    }

    case class RealIndexReferenceBuilder(
                                      indexRegister: GeneralPurposeRegister & RealModeIndexRegister & WordSize,
                                      override val segment: SegmentRegister,
                                    ) extends RealModeReferenceBuilder {
      override def base: Option[GeneralPurposeRegister & RealModeBaseRegister & WordSize] = None

      override def index: Option[GeneralPurposeRegister & RealModeIndexRegister & WordSize] = Some(indexRegister)

      override def displacement: Int = 0

      @targetName("withDisplacement")
      def +(displacementValue: Int): RealDisplacementReferenceBuilder =
        RealDisplacementReferenceBuilder(base, index, segment, displacementValue)
    }

    case class RealBaseIndexReferenceBuilder(
                                          baseRegister: GeneralPurposeRegister & RealModeBaseRegister & WordSize,
                                          indexRegister: GeneralPurposeRegister & RealModeIndexRegister & WordSize,
                                          override val segment: SegmentRegister
                                        ) extends RealModeReferenceBuilder {
      override def base: Option[GeneralPurposeRegister & RealModeBaseRegister & WordSize] = Some(baseRegister)

      override def index: Option[GeneralPurposeRegister & RealModeIndexRegister & WordSize] = Some(indexRegister)

      override def displacement: Int = 0

      @targetName("withDisplacement")
      def +(displacementValue: Int): RealDisplacementReferenceBuilder =
        RealDisplacementReferenceBuilder(base, index, segment, displacementValue)
    }

    case class RealDisplacementReferenceBuilder(
                                             override val base: Option[GeneralPurposeRegister & RealModeBaseRegister & WordSize],
                                             override val index: Option[GeneralPurposeRegister & RealModeIndexRegister & WordSize],
                                             override val segment: SegmentRegister,
                                             override val displacement: Int = 0
                                           ) extends RealModeReferenceBuilder



  }


  trait I386Operations {
    self: I386GenericRegisters & ImmediateValue.I386Implicits =>

    class ProtectedDestinationReference(
                                         reference: RegisterReference & DestinationIndex & RealModeIndexRegister & WordSize,
                                         segment: SegmentRegister,
                                       )(using AddressSizePrefixRequirement) extends IndirectMemoryLocation(
      7.toByte, 0.toByte, None, None, segment, Set(AddressOperandInfo.rmIndex(reference, None)), reference.defaultSegment
    ) {
      self: ValueSize =>
    }

    class ProtectedSourceReference(
                                    reference: RegisterReference & SourceIndex & RealModeIndexRegister & WordSize,
                                    segment: SegmentRegister,
                                  )(using AddressSizePrefixRequirement) extends IndirectMemoryLocation(
      6.toByte, 0.toByte, None, None, segment, Set(AddressOperandInfo.rmIndex(reference, None)), reference.defaultSegment
    ) {
      self: ValueSize =>
    }


    class ProtectedIndirectMemoryLocation(
                                           referenceBuilder: ProtectedModeReferenceBuilder
                                         )(using AddressSizePrefixRequirement) extends IndirectMemoryLocation(
      referenceBuilder.registerOrMemoryModeCode,
      referenceBuilder.modValue,
      referenceBuilder.displacementValue,
      referenceBuilder.sibValue,
      referenceBuilder.segment,
      referenceBuilder.addressOperands,
      referenceBuilder.index.map(_.defaultSegment).getOrElse(Segment.Data),
    ) {
      self: ValueSize =>

      override def toString: String =
        s"$sizeName PTR ${referenceBuilder.toString}"
    }

    sealed abstract class ProtectedModeReferenceBuilder extends ReferenceBuilder {
      def base: Option[GeneralPurposeRegister & SIBBaseRegister & ProtectedModeIndexRegister & DoubleQuadSize]

      def index: Option[GeneralPurposeRegister & ProtectedModeIndexRegister & DoubleQuadSize]

      def displacement: Int

      def multiplier: Int

      def segment: SegmentRegister

      final def defaultSegment: SegmentRegister =
        index.map(_.defaultSegment).getOrElse(base.map(_.defaultSegment).getOrElse(Segment.Data))

      private[I386Operations] def registerOrMemoryModeCode: Byte = {
        (base, index) match {
          case (None, Some(index)) if multiplier == 1 => index.registerOrMemoryModeCode
          case (None, None) => 6.toByte
          case _ => 4.toByte
        }
      }

      private[I386Operations] def modValue =
        if base.isEmpty && index.isEmpty then 0.toByte
        else if displacement == 0 then
          if index.exists(_.isInstanceOf[BasePointer]) &&
            base.isEmpty || base.exists(_.isInstanceOf[BasePointer]) then
            1.toByte
          else 0.toByte
        else if displacement < 256 then 1.toByte
        else 2.toByte

      private[I386Operations] def displacementValue =
        if base.isEmpty && index.isEmpty then
          Some(doubleWordImmediate(multiplier))
        else if displacement == 0 then
          if index.exists(_.isInstanceOf[BasePointer]) && base.isEmpty || base.exists(_.isInstanceOf[BasePointer]) then
            Some(byteImmediate(0))
          else None
        else if displacement < 256 then Some(byteImmediate(displacement.toByte))
        else Some(doubleWordImmediate(displacement))

      private[I386Operations] def scaleValue: Byte = multiplier match {
        case 1 => 0.toByte
        case 2 => 1.toByte
        case 4 => 2.toByte
        case 8 => 3.toByte
        case _ => throw new AssertionError(s"$multiplier is not a valid scale value, it can only be one of (1, 2, 4, 8)")
      }

      private[I386Operations] def sibValue: Option[Byte] =
        (base, index) match {
          case (None, Some(index)) if multiplier != 1 =>
            Some((scaleValue << 6 + index.registerOrMemoryModeCode << 3 + 6).toByte)
          case (Some(base), indexOption) =>
            Some((scaleValue << 6 + indexOption.map(i => i.registerOrMemoryModeCode << 3).getOrElse(5) + base.registerOrMemoryModeCode).toByte)
          case _ =>
            None
        }

      private[I386Operations] def addressOperands(using AddressSizePrefixRequirement) =
        base.map(b => AddressOperandInfo.SIBBase(b)).toSet ++
          index.map(i => AddressOperandInfo.rmIndex(i, Option.when(segment != defaultSegment)(segment))).toSet

      override def toString: String = {
        val seg = if segment != defaultSegment then s"$segment:" else ""
        val baseIndex = (base, index) match {
          case (Some(base), Some(index)) if displacement == 0 =>
            s"$base+$index"
          case (Some(base), Some(index)) =>
            s"$base+$index+$displacement"
          case (Some(base), None) if displacement == 0 =>
            s"$base"
          case (Some(base), None) =>
            s"$base+$displacement"
          case (None, Some(index)) if displacement == 0 =>
            s"$index"
          case (None, Some(index)) =>
            s"$index+$displacement"
          case (None, None) if displacement == 0 =>
            s""
          case (None, None) =>
            displacement.toString
        }

        s"$seg[$baseIndex]"
      }
    }

    case class ProtectedBaseReferenceBuilder(
                                              baseRegister: GeneralPurposeRegister & SIBBaseRegister & ProtectedModeIndexRegister & DoubleQuadSize,
                                              override val segment: SegmentRegister
                                            ) extends ProtectedModeReferenceBuilder {
      override def base: Option[GeneralPurposeRegister & SIBBaseRegister & ProtectedModeIndexRegister & DoubleQuadSize] = None

      override def index: Option[GeneralPurposeRegister & ProtectedModeIndexRegister & DoubleQuadSize] = Some(baseRegister)

      override def displacement: Int = 0

      override def multiplier: Int = 1

      @targetName("withDisplacement")
      def +(displacementValue: Int): ProtectedDisplacementReferenceBuilder =
        ProtectedDisplacementReferenceBuilder(base, index, segment, 1, displacementValue)
    }

    case class ProtectedBaseIndexReferenceBuilder(
                                                   baseRegister: GeneralPurposeRegister & SIBBaseRegister & ProtectedModeIndexRegister & DoubleQuadSize,
                                                   indexRegister: GeneralPurposeRegister & SIBBaseRegister & ProtectedModeIndexRegister & DoubleQuadSize,
                                                   override val segment: SegmentRegister
                                                 ) extends ProtectedModeReferenceBuilder {
      override def base: Option[GeneralPurposeRegister & SIBBaseRegister & ProtectedModeIndexRegister & DoubleQuadSize] = Some(baseRegister)

      override def index: Option[GeneralPurposeRegister & ProtectedModeIndexRegister & DoubleQuadSize] = Some(indexRegister)

      override def displacement: Int = 0

      override def multiplier: Int = 1

      @targetName("withDisplacement")
      def +(displacementValue: Int): ProtectedDisplacementReferenceBuilder =
        ProtectedDisplacementReferenceBuilder(base, index, segment, 1, displacementValue)
    }

    case class ProtectedDisplacementReferenceBuilder(
                                              base: Option[GeneralPurposeRegister & SIBBaseRegister & ProtectedModeIndexRegister & DoubleQuadSize],
                                              index: Option[GeneralPurposeRegister & ProtectedModeIndexRegister & DoubleQuadSize],
                                              override val segment: SegmentRegister,
                                              override val multiplier: Int,
                                              override val displacement: Int,
                                            ) extends ProtectedModeReferenceBuilder



    @targetName("asReference")
    given Conversion[GeneralPurposeRegister & SIBBaseRegister & ProtectedModeIndexRegister & DoubleQuadSize, ProtectedBaseReferenceBuilder] =
      baseRegister => ProtectedBaseReferenceBuilder(baseRegister, baseRegister.defaultSegment)

  }

  trait LegacyOperations extends I8086Operations {
    self: I8086SpecificRegisters & ImmediateValue.I8086Implicits =>

    given indirectForByteSize(using AddressSizePrefixRequirement): IndirectMemoryLocationForSize[ByteSize] with {
      override def instance(referenceBuilder: ReferenceBuilder): IndirectMemoryLocation & ByteSize =
        new RealIndirectMemoryLocation(referenceBuilder.asInstanceOf[RealModeReferenceBuilder]) with ByteSize

      override def destination(reference: RegisterReference & DestinationIndex & IndexRegister & WordDoubleQuadSize, segment: SegmentRegister): DestinationReference & ByteSize =
        new DestinationReference(reference, segment) with ByteSize

      override def source(reference: RegisterReference & SourceIndex & IndexRegister & WordDoubleQuadSize, segment: SegmentRegister): SourceReference & ByteSize =
        new SourceReference(reference, segment) with ByteSize

    }

    given indirectForWordSize(using AddressSizePrefixRequirement): IndirectMemoryLocationForSize[WordSize] with {
      override def instance(referenceBuilder: ReferenceBuilder): IndirectMemoryLocation & WordSize =
        new RealIndirectMemoryLocation(referenceBuilder.asInstanceOf[RealModeReferenceBuilder]) with WordSize

      override def destination(reference: RegisterReference & DestinationIndex & IndexRegister & WordDoubleQuadSize, segment: SegmentRegister): DestinationReference & WordSize =
        new DestinationReference(reference, segment) with WordSize

      override def source(reference: RegisterReference & SourceIndex & IndexRegister & WordDoubleQuadSize, segment: SegmentRegister): SourceReference & WordSize =
        new SourceReference(reference, segment) with WordSize
    }

    extension (seg: SegmentRegister)
      @targetName("prefixes")
      def /(baseRegister: GeneralPurposeRegister & RealModeBaseRegister & RealModeIndexRegister & WordSize): RealBaseReferenceBuilder =
        RealBaseReferenceBuilder(baseRegister, seg)

      @targetName("prefixes")
      def /(indexRegister: GeneralPurposeRegister & RealModeIndexRegister & WordSize): RealIndexReferenceBuilder =
        RealIndexReferenceBuilder(indexRegister, seg)

    def RegisterMemoryLocation[Size <: ValueSize : IndirectMemoryLocationForSize](reference: RealModeReferenceBuilder)(using AddressSizePrefixRequirement): IndirectMemoryLocation & Size =
      summon[IndirectMemoryLocationForSize[Size]].instance(reference)

    def DestinationReference[Size <: ValueSize : IndirectMemoryLocationForSize](reference: RegisterReference & DestinationIndex & IndexRegister & WordSize, displacement: Option[ImmediateValue[?] & ByteWordSize] = None, segment: Option[SegmentRegister] = None)(implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement): DestinationReference & Size =
      summon[IndirectMemoryLocationForSize[Size]].destination(reference, segment.getOrElse(reference.defaultSegment))

    def SourceReference[Size <: ValueSize : IndirectMemoryLocationForSize](reference: RegisterReference & SourceIndex & IndexRegister & WordSize, displacement: Option[ImmediateValue[?] & ByteWordSize] = None, segment: Option[SegmentRegister] = None)(implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement): SourceReference & Size =
      summon[IndirectMemoryLocationForSize[Size]].source(reference, segment.getOrElse(reference.defaultSegment))
  }

  trait RealOperations extends I8086Operations, I386Operations {
    self: I386GenericRegisters & ImmediateValue.I386Implicits & I8086SpecificRegisters & ImmediateValue.I8086Implicits =>

    given indirectForByteSize(using AddressSizePrefixRequirement): IndirectMemoryLocationForSize[ByteSize] with {
      override def instance(referenceBuilder: ReferenceBuilder): IndirectMemoryLocation & ByteSize =
        referenceBuilder match {
          case ref: RealModeReferenceBuilder =>
            new RealIndirectMemoryLocation(ref) with ByteSize
          case ref: ProtectedModeReferenceBuilder =>
            new ProtectedIndirectMemoryLocation(ref) with ByteSize
        }

      override def destination(reference: RegisterReference & DestinationIndex & IndexRegister & WordDoubleQuadSize, segment: SegmentRegister): DestinationReference & ByteSize =
        new DestinationReference(reference, segment) with ByteSize

      override def source(reference: RegisterReference & SourceIndex & IndexRegister & WordDoubleQuadSize, segment: SegmentRegister): SourceReference & ByteSize =
        new SourceReference(reference, segment) with ByteSize
    }

    given indirectForWordSize(using AddressSizePrefixRequirement): IndirectMemoryLocationForSize[WordSize] with {
      override def instance(referenceBuilder: ReferenceBuilder): IndirectMemoryLocation & WordSize =
        referenceBuilder match {
          case ref: RealModeReferenceBuilder =>
            new RealIndirectMemoryLocation(ref) with WordSize
          case ref: ProtectedModeReferenceBuilder =>
            new ProtectedIndirectMemoryLocation(ref) with WordSize
        }

      override def destination(reference: RegisterReference & DestinationIndex & IndexRegister & WordDoubleQuadSize, segment: SegmentRegister): DestinationReference & WordSize =
        new DestinationReference(reference, segment) with WordSize

      override def source(reference: RegisterReference & SourceIndex & IndexRegister & WordDoubleQuadSize, segment: SegmentRegister): SourceReference & WordSize =
        new SourceReference(reference, segment) with WordSize
    }

    given indirectForDoubleWordSize(using AddressSizePrefixRequirement): IndirectMemoryLocationForSize[DoubleWordSize] with {
      override def instance(referenceBuilder: ReferenceBuilder): IndirectMemoryLocation & DoubleWordSize =
        referenceBuilder match {
          case ref: RealModeReferenceBuilder =>
            new RealIndirectMemoryLocation(ref) with DoubleWordSize
          case ref: ProtectedModeReferenceBuilder =>
            new ProtectedIndirectMemoryLocation(ref) with DoubleWordSize
        }

      override def destination(reference: RegisterReference & DestinationIndex & IndexRegister & WordDoubleQuadSize, segment: SegmentRegister): DestinationReference & DoubleWordSize =
        new DestinationReference(reference, segment) with DoubleWordSize

      override def source(reference: RegisterReference & SourceIndex & IndexRegister & WordDoubleQuadSize, segment: SegmentRegister): SourceReference & DoubleWordSize =
        new SourceReference(reference, segment) with DoubleWordSize
    }

    extension (seg: SegmentRegister)
      @targetName("prefixes")
      def /(baseRegister: GeneralPurposeRegister & RealModeBaseRegister & WordSize): RealBaseReferenceBuilder =
        RealBaseReferenceBuilder(baseRegister, seg)

      @targetName("prefixes")
      def /(indexRegister: GeneralPurposeRegister & RealModeIndexRegister & WordSize): RealIndexReferenceBuilder =
        RealIndexReferenceBuilder(indexRegister, seg)

      @targetName("prefixes")
      def /(baseRegister: GeneralPurposeRegister & SIBBaseRegister & ProtectedModeIndexRegister & DoubleQuadSize): ProtectedBaseReferenceBuilder =
        ProtectedBaseReferenceBuilder(baseRegister, seg)

    def RegisterMemoryLocation[Size <: ValueSize : IndirectMemoryLocationForSize](reference: ReferenceBuilder)(using AddressSizePrefixRequirement): IndirectMemoryLocation & Size =
      summon[IndirectMemoryLocationForSize[Size]].instance(reference)

    def DestinationReference[Size <: ValueSize : IndirectMemoryLocationForSize](reference: RegisterReference & DestinationIndex & IndexRegister & WordDoubleSize, displacement: Option[ImmediateValue[?] & ByteWordDoubleSize] = None, segment: Option[SegmentRegister] = None)(implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement): DestinationReference & Size =
      summon[IndirectMemoryLocationForSize[Size]].destination(reference, segment.getOrElse(reference.defaultSegment))

    def SourceReference[Size <: ValueSize : IndirectMemoryLocationForSize](reference: RegisterReference & SourceIndex & IndexRegister & WordDoubleSize, displacement: Option[ImmediateValue[?] & ByteWordDoubleSize] = None, segment: Option[SegmentRegister] = None)(implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement): SourceReference & Size =
      summon[IndirectMemoryLocationForSize[Size]].source(reference, segment.getOrElse(reference.defaultSegment))
  }

  trait ProtectedOperations extends RealOperations {
    self: I386GenericRegisters & ImmediateValue.I386Implicits & I8086SpecificRegisters & ImmediateValue.I8086Implicits =>
  }

  trait LongOperations extends I386Operations {
    self: I386GenericRegisters & ImmediateValue.I386Implicits =>
    given indirectForByteSize(using AddressSizePrefixRequirement): IndirectMemoryLocationForSize[ByteSize] with {
      override def instance(referenceBuilder: ReferenceBuilder): IndirectMemoryLocation & ByteSize =
        new ProtectedIndirectMemoryLocation(referenceBuilder.asInstanceOf[ProtectedModeReferenceBuilder]) with ByteSize

      override def destination(reference: RegisterReference & DestinationIndex & IndexRegister & WordDoubleQuadSize, segment: SegmentRegister): DestinationReference & ByteSize =
        new DestinationReference(reference, segment) with ByteSize

      override def source(reference: RegisterReference & SourceIndex & IndexRegister & WordDoubleQuadSize, segment: SegmentRegister): SourceReference & ByteSize =
        new SourceReference(reference, segment) with ByteSize
    }

    given indirectForWordSize(using AddressSizePrefixRequirement): IndirectMemoryLocationForSize[WordSize] with {
      override def instance(referenceBuilder: ReferenceBuilder): IndirectMemoryLocation & WordSize =
        new ProtectedIndirectMemoryLocation(referenceBuilder.asInstanceOf[ProtectedModeReferenceBuilder]) with WordSize

      override def destination(reference: RegisterReference & DestinationIndex & IndexRegister & WordDoubleQuadSize, segment: SegmentRegister): DestinationReference & WordSize =
        new DestinationReference(reference, segment) with WordSize

      override def source(reference: RegisterReference & SourceIndex & IndexRegister & WordDoubleQuadSize, segment: SegmentRegister): SourceReference & WordSize =
        new SourceReference(reference, segment) with WordSize
    }

    given indirectForDoubleWordSize(using AddressSizePrefixRequirement): IndirectMemoryLocationForSize[DoubleWordSize] with {
      override def instance(referenceBuilder: ReferenceBuilder): IndirectMemoryLocation & DoubleWordSize =
        new ProtectedIndirectMemoryLocation(referenceBuilder.asInstanceOf[ProtectedModeReferenceBuilder]) with DoubleWordSize

      override def destination(reference: RegisterReference & DestinationIndex & IndexRegister & WordDoubleQuadSize, segment: SegmentRegister): DestinationReference & DoubleWordSize =
        new DestinationReference(reference, segment) with DoubleWordSize

      override def source(reference: RegisterReference & SourceIndex & IndexRegister & WordDoubleQuadSize, segment: SegmentRegister): SourceReference & DoubleWordSize =
        new SourceReference(reference, segment) with DoubleWordSize
    }

    given indirectForQuadWordSize(using AddressSizePrefixRequirement): IndirectMemoryLocationForSize[QuadWordSize] with {
      override def instance(referenceBuilder: ReferenceBuilder): IndirectMemoryLocation & QuadWordSize =
        new ProtectedIndirectMemoryLocation(referenceBuilder.asInstanceOf[ProtectedModeReferenceBuilder]) with QuadWordSize

      override def destination(reference: RegisterReference & DestinationIndex & IndexRegister & WordDoubleQuadSize, segment: SegmentRegister): DestinationReference & QuadWordSize =
        new DestinationReference(reference, segment) with QuadWordSize

      override def source(reference: RegisterReference & SourceIndex & IndexRegister & WordDoubleQuadSize, segment: SegmentRegister): SourceReference & QuadWordSize =
        new SourceReference(reference, segment) with QuadWordSize
    }

    extension (seg: SegmentRegister)
      @targetName("prefixes")
      def /(baseRegister: GeneralPurposeRegister & SIBBaseRegister & ProtectedModeIndexRegister & DoubleQuadSize): ProtectedBaseReferenceBuilder =
        ProtectedBaseReferenceBuilder(baseRegister, seg)

    def RegisterMemoryLocation[Size <: ValueSize : IndirectMemoryLocationForSize](reference: ProtectedModeReferenceBuilder)(using AddressSizePrefixRequirement): IndirectMemoryLocation & Size =
      summon[IndirectMemoryLocationForSize[Size]].instance(reference)

    def DestinationReference[Size <: ValueSize : IndirectMemoryLocationForSize](reference: RegisterReference & DestinationIndex & IndexRegister & DoubleQuadSize, displacement: Option[ImmediateValue[?] & ByteWordSize] = None, segment: Option[SegmentRegister] = None)(implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement): DestinationReference & Size =
      summon[IndirectMemoryLocationForSize[Size]].destination(reference, segment.getOrElse(reference.defaultSegment))

    def SourceReference[Size <: ValueSize : IndirectMemoryLocationForSize](reference: RegisterReference & SourceIndex & IndexRegister & DoubleQuadSize, displacement: Option[ImmediateValue[?] & ByteWordSize] = None, segment: Option[SegmentRegister] = None)(implicit byteImmediate: ValueToByteImmediate, addressSizePrefixRequirement: AddressSizePrefixRequirement): SourceReference & Size =
      summon[IndirectMemoryLocationForSize[Size]].source(reference, segment.getOrElse(reference.defaultSegment))
  }
}