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

package org.werner.absynt.x86.operands.registers

import org.werner.absynt.x86.operands.{WordDoubleQuadSize, WordDoubleSize, WordSize}

sealed trait RMRegisterReference[Size <: WordDoubleQuadSize] {
  val base: Option[GeneralPurposeRegister with RealRMBaseRegister with Size]
  val index: GeneralPurposeRegister with RMIndexRegister with Size
  val indexCode: Byte
  def segment: SegmentRegister
}
sealed case class DestinationIndexReference[Size <: WordDoubleQuadSize](destinationIndex: DestinationIndex with RMIndexRegister with Size) extends RMRegisterReference[Size] {
  override val base: Option[GeneralPurposeRegister with RealRMBaseRegister with Size] = None
  override val index: GeneralPurposeRegister with RMIndexRegister with Size = destinationIndex
  override val indexCode: Byte = destinationIndex.indexCode
  override def segment: SegmentRegister = destinationIndex.defaultSegment
  override def toString: String = index.toString
}

sealed case class SourceIndexReference[Size <: WordDoubleQuadSize](sourceIndex: SourceIndex with RMIndexRegister with Size) extends RMRegisterReference[Size] {
  override val base: Option[GeneralPurposeRegister with RealRMBaseRegister with Size] = None
  override val index: GeneralPurposeRegister with RMIndexRegister with Size = sourceIndex
  override val indexCode: Byte = sourceIndex.indexCode
  override def segment: SegmentRegister = sourceIndex.defaultSegment
  override def toString: String = index.toString
}

sealed trait RMIndexRegister {
  self: GeneralPurposeRegister =>
  val indexCode: Byte
  def defaultSegment: SegmentRegister
}

trait RealRMIndexRegister extends RMIndexRegister {
  self: GeneralPurposeRegister =>
}

trait CombinableRealRMIndexRegister extends RealRMIndexRegister {
  self: GeneralPurposeRegister =>

  def +(base: RealRMBaseRegister): BaseIndexReference[WordSize] =
    base.combinedIndex(this)
}

trait RealRMBaseRegister {
  self: GeneralPurposeRegister =>

  def combinedIndex(index: CombinableRealRMIndexRegister): BaseIndexReference[WordSize]

  final def +(index: CombinableRealRMIndexRegister): BaseIndexReference[WordSize] =
    combinedIndex(index)
}

trait ProtectedRMIndexRegister extends RMIndexRegister {
  self: GeneralPurposeRegister =>
  override val indexCode: Byte = self.registerOrMemoryModeCode
  override def defaultSegment: SegmentRegister = Segment.Data
}

sealed class BaseIndexReference[Size <: WordDoubleQuadSize](
  val base: Option[GeneralPurposeRegister with RealRMBaseRegister with Size],
  val index: GeneralPurposeRegister with RMIndexRegister with Size,
  override val indexCode: Byte,
  override val segment: SegmentRegister
)
extends RMRegisterReference[Size] {
  override def toString = s"${base.map(_+"+").getOrElse("")}$index"
}

object BaseIndexReference {
  def apply[Size <: WordDoubleQuadSize](
    base: Option[GeneralPurposeRegister with RealRMBaseRegister with Size],
    index: GeneralPurposeRegister with RMIndexRegister with Size,
    indexCode: Byte,
    segment: SegmentRegister,
  ) = new BaseIndexReference[Size](base, index, indexCode, segment)


  object BX_SI extends BaseIndexReference[WordSize](Some(Base.Word), SourceIndex.Real, 0x00, SourceIndex.Real.defaultSegment)
  object BX_DI extends BaseIndexReference[WordSize](Some(Base.Word), DestinationIndex.Real, 0x01, DestinationIndex.Real.defaultSegment)
  object BP_SI extends BaseIndexReference[WordSize](Some(BasePointer.Real), SourceIndex.Real, 0x02, SourceIndex.Real.defaultSegment)
  object BP_DI extends BaseIndexReference[WordSize](Some(BasePointer.Real), DestinationIndex.Real, 0x03, DestinationIndex.Real.defaultSegment)
}
