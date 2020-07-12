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

import MemoryLocation.BaseIndexReference
import org.werner.absynt.x86.operands.registers._
import org.werner.absynt.x86.operands.{ModRMEncodableOperand, _}
import org.werner.absynt.x86.operations.{AddressOperandInfo, AddressSizePrefixRequirement}

sealed abstract class SIBMemoryLocation[AddressSize <: WordDoubleQuadSize](
  val reference: BaseIndexReference[
    GeneralPurposeRegister with BasePointerRegister with AddressSize,
    GeneralPurposeRegister with IndexRegister with AddressSize,
    AddressSize,
  ]
) extends MemoryLocation(
      if (!SIBMemoryLocation.requiresDisplacement(reference))
        None
      else if (reference.scale > 1 && reference.base.isEmpty)
        Some(ImmediateValue.doubleWordImmediate(reference.displacement.toInt))
      else if (reference.displacement.toByte == reference.displacement)
        Some(ImmediateValue.byteImmediate(reference.displacement.toByte))
      else
        Some(ImmediateValue.doubleWordImmediate(reference.displacement.toInt)),
      reference.segment,
    )
    with ModRMEncodableOperand {

  self: ValueSize =>

  val modValue: Byte = {
    (displacement, reference.base, reference.index) match {
      case (Some(_), None, _) if reference.scale > 1 => 0x00
      case (None, _, _)                              => 0x00
      case (Some(_: ByteSize), _, _)                 => 0x01
      case (Some(_: DoubleWordSize), _, _)           => 0x02
    }
  }

  override val registerOrMemoryModeCode: Byte =
    (reference.base, reference.index) match {
      case (None, Some(index)) if !requiresSIB => index.indexCode
      case (None, None)                        => ???
      case _                                   => 0x04.toByte
    }

  assume((1 :: 2 :: 4 :: 8 :: Nil).contains(reference.scale))

  override val defaultSegment: SegmentRegister =
    reference.base.map(_.defaultSegment).orElse(reference.index.map(_.defaultSegment)).getOrElse(Segment.Data)

  val baseCode: Byte = reference.base.map(_.registerOrMemoryModeCode).getOrElse(5.toByte)
  val indexCode: Byte = reference.index.map(_.registerOrMemoryModeCode).getOrElse(4)

  override def addressOperands(implicit
    addressSizePrefixRequirement: AddressSizePrefixRequirement
  ): Set[AddressOperandInfo] =
    if (requiresSIB)
    (reference.base.map(AddressOperandInfo.SIBBase) ++ reference.index.map(i =>
      AddressOperandInfo.SIBIndex(i, segmentOverride)
    )).toSet
    else
      reference.index.map(i => AddressOperandInfo.rmIndex(i, segmentOverride)).toSet


  def requiresSIB: Boolean = reference.base.isDefined || reference.scale > 1

  override def getExtendedBytes(rValue: Byte): Seq[Byte] = {
    if (requiresSIB)
      super.getExtendedBytes(rValue) ++ (getSIB +: displacement.toSeq.flatMap(_.encodedValue))
    else
      super.getExtendedBytes(rValue) ++ (displacement.toSeq.flatMap(_.encodedValue))
  }

  def getSIB: Byte = {
    val scaleCode = reference.scale match {
      case 1 => 0x0
      case 2 => 0x1
      case 4 => 0x2
      case 8 => 0x3
    }
    ((scaleCode << 6) | (indexCode << 3) | baseCode).toByte
  }

  override def toString: String = s"$sizeName PTR $segmentPrefix[$reference]"
}

object SIBMemoryLocation {

  private def requiresDisplacement(reference: BaseIndexReference[_,_,_]): Boolean =
    (
      reference.base.contains(BasePointer.Protected) || reference.base.contains(BasePointer.Long)) ||
      (reference.base.isEmpty && (reference.index.contains(BasePointer.Protected) || reference.index.contains(BasePointer.Long) || reference.scale > 1) ||
      reference.displacement != 0
    )


  abstract class SIBForSize[Size <: ValueSize]() {

    def instance[AddressSize <: DoubleQuadSize](
      reference: BaseIndexReference[
        GeneralPurposeRegister with BasePointerRegister with AddressSize,
        GeneralPurposeRegister with IndexRegister with AddressSize,
        AddressSize,
      ]
    ): SIBMemoryLocation[AddressSize] with Size
  }

  trait I386Implicits {

    implicit def SIBforByteSize: SIBForSize[ByteSize] =
      new SIBForSize[ByteSize] {

        override def instance[AddressSize <: DoubleQuadSize](
          reference: BaseIndexReference[
            GeneralPurposeRegister with BasePointerRegister with AddressSize,
            GeneralPurposeRegister with IndexRegister with AddressSize,
            AddressSize,
          ]
        ): SIBMemoryLocation[AddressSize] with ByteSize =
          new SIBMemoryLocation(reference) with ByteSize
      }

    implicit def SIBforWordSize: SIBForSize[WordSize] =
      new SIBForSize[WordSize] {

        override def instance[AddressSize <: DoubleQuadSize](
          reference: BaseIndexReference[
            GeneralPurposeRegister with BasePointerRegister with AddressSize,
            GeneralPurposeRegister with IndexRegister with AddressSize,
            AddressSize,
          ]
        ): SIBMemoryLocation[AddressSize] with WordSize =
          new SIBMemoryLocation(reference) with WordSize
      }

    implicit def SIBforDoubleWordSize: SIBForSize[DoubleWordSize] =
      new SIBForSize[DoubleWordSize] {

        override def instance[AddressSize <: DoubleQuadSize](
          reference: BaseIndexReference[
            GeneralPurposeRegister with BasePointerRegister with AddressSize,
            GeneralPurposeRegister with IndexRegister with AddressSize,
            AddressSize,
          ]
        ): SIBMemoryLocation[AddressSize] with DoubleWordSize =
          new SIBMemoryLocation(reference) with DoubleWordSize
      }
  }

  trait I386Operations extends I386Implicits {}

  trait LongOperations extends I386Implicits {

    implicit def SIBforQuadWordSize: SIBForSize[QuadWordSize] =
      new SIBForSize[QuadWordSize] {

        override def instance[AddressSize <: DoubleQuadSize](
          reference: BaseIndexReference[
            GeneralPurposeRegister with BasePointerRegister with AddressSize,
            GeneralPurposeRegister with IndexRegister with AddressSize,
            AddressSize,
          ]
        ): SIBMemoryLocation[AddressSize] with QuadWordSize =
          new SIBMemoryLocation(reference) with QuadWordSize
      }

  }

}
