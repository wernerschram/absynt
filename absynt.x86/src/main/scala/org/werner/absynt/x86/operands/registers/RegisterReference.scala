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

import org.werner.absynt.x86.operands.memoryaccess.{LongSIB, ProtectedSIB}
import org.werner.absynt.x86.operands.{DoubleWordSize, ImmediateValue, ModRMEncodableOperand, QuadWordSize, WordSize}

sealed trait RegisterReference {
  val defaultSegment: SegmentRegister = Segment.Data
  val indexCode: Byte

  def onlyWithDisplacement: Boolean = false
}

sealed trait IndexRegister extends RegisterReference {
  self: GeneralPurposeRegister =>
}

trait RealModeIndexRegister extends IndexRegister {
  self: GeneralPurposeRegister =>
}

trait CombinableRealModeIndexRegister extends RealModeIndexRegister {
  self: GeneralPurposeRegister =>

  def +(base: RealModeBaseRegister): BaseIndexReference =
    base.combinedIndex(this)
}

trait RealModeBaseRegister extends ModRMEncodableOperand {
  self: GeneralPurposeRegister =>

  def combinedIndex(index: CombinableRealModeIndexRegister): BaseIndexReference

  final def +(index: CombinableRealModeIndexRegister): BaseIndexReference =
    combinedIndex(index)
}

trait ProtectedModeIndexRegister extends IndexRegister {
  self: GeneralPurposeRegister =>
  override val indexCode: Byte = self.registerOrMemoryModeCode
}

sealed trait SIBIndexRegister extends ModRMEncodableOperand {
  val defaultSIBSegment: SegmentRegister = Segment.Data
  val SIBIndexCode: Byte = registerOrMemoryModeCode
}

trait ProtectedSIBIndexRegister extends SIBIndexRegister {
  self: GeneralPurposeRegister with DoubleWordSize =>

  final def *(newFactor: Int): ProtectedSIBIndexReference =
    ProtectedSIBIndexReference(this, newFactor, None)
}

trait LongSIBIndexRegister extends SIBIndexRegister {
  self: GeneralPurposeRegister with QuadWordSize =>

  final def *(newFactor: Int): LongSIBIndexReference =
    LongSIBIndexReference(this, newFactor, None)

}

sealed trait SIBBaseRegister extends ModRMEncodableOperand {
  self: GeneralPurposeRegister =>

  val SIBBaseCode: Byte = registerOrMemoryModeCode
}

trait ProtectedSIBBaseRegister extends SIBBaseRegister with ProtectedSIB {
  self: GeneralPurposeRegister with DoubleWordSize =>
  override def base: Option[GeneralPurposeRegister with ProtectedSIBBaseRegister with DoubleWordSize] = Some(self)
  override def index: Option[GeneralPurposeRegister with ProtectedSIBIndexRegister with DoubleWordSize] = None

  override def scale: Int = 1.toByte
  override def displacement: Option[ImmediateValue[Int] with DoubleWordSize] = None

  override def segment: SegmentRegister = Segment.Data

  final def +(
    sib: GeneralPurposeRegister with ProtectedSIBIndexRegister with DoubleWordSize): ProtectedSIBBaseIndexReference =
    ProtectedSIBBaseIndexReference(this, Some(sib))

  final def +(sib: ProtectedSIBIndexReference): ProtectedSIBBaseIndexReference =
    ProtectedSIBBaseIndexReference(this, sib.index, sib.scale, sib.displacement)

  final def +(displacement: ImmediateValue[Int] with DoubleWordSize) =
    ProtectedSIBBaseIndexReference(this, None, 1, Some(displacement))
}

trait LongSIBBaseRegister extends SIBBaseRegister with LongSIB {
  self: GeneralPurposeRegister with QuadWordSize =>
  override def base: Option[GeneralPurposeRegister with LongSIBBaseRegister with QuadWordSize] = Some(self)
  override def index: Option[GeneralPurposeRegister with LongSIBIndexRegister with QuadWordSize] = None

  override def scale: Int = 1.toByte
  override def displacement: Option[ImmediateValue[Int] with DoubleWordSize] = None

  override def segment: SegmentRegister = Segment.Data

  final def +(sib: GeneralPurposeRegister with LongSIBIndexRegister with QuadWordSize): LongSIBBaseIndexReference =
    LongSIBBaseIndexReference(this, Some(sib))

  final def +(sib: LongSIBIndexReference): LongSIBBaseIndexReference =
    LongSIBBaseIndexReference(this, sib.index, sib.scale, sib.displacement)

  final def +(displacement: ImmediateValue[Int] with DoubleWordSize) =
    LongSIBBaseIndexReference(this, None, 1, Some(displacement))

}

case class ProtectedSIBIndexReference(
  indexRegister: GeneralPurposeRegister with ProtectedSIBIndexRegister with DoubleWordSize,
  scale: Int = 1,
  displacement: Option[ImmediateValue[Int] with DoubleWordSize] = None,
  segment: SegmentRegister = Segment.Data
) extends ProtectedSIB {
  override def base: Option[GeneralPurposeRegister with ProtectedSIBBaseRegister with DoubleWordSize] = None
  override def index: Option[GeneralPurposeRegister with ProtectedSIBIndexRegister with DoubleWordSize] =
    Some(indexRegister)

  final def +(newDisplacement: ImmediateValue[Int] with DoubleWordSize): ProtectedSIBIndexReference =
    this.copy(
      displacement =
        Some(ImmediateValue.doubleWordImmediate(displacement.map(_.value).getOrElse(0) + newDisplacement.value)))

  final def -(newDisplacement: ImmediateValue[Int] with DoubleWordSize): ProtectedSIBIndexReference =
    this.copy(
      displacement =
        Some(ImmediateValue.doubleWordImmediate(displacement.map(_.value).getOrElse(0) - newDisplacement.value)))
}

case class LongSIBIndexReference(
  indexRegister: GeneralPurposeRegister with LongSIBIndexRegister with QuadWordSize,
  scale: Int = 1,
  displacement: Option[ImmediateValue[Int] with DoubleWordSize] = None,
  segment: SegmentRegister = Segment.Data
) extends LongSIB {
  override def base: Option[GeneralPurposeRegister with LongSIBBaseRegister with QuadWordSize] = None
  override def index: Option[GeneralPurposeRegister with LongSIBIndexRegister with QuadWordSize] = Some(indexRegister)

  final def +(newDisplacement: ImmediateValue[Int] with DoubleWordSize): LongSIBIndexReference =
    this.copy(
      displacement =
        Some(ImmediateValue.doubleWordImmediate(displacement.map(_.value).getOrElse(0) + newDisplacement.value)))

  final def -(newDisplacement: ImmediateValue[Int] with DoubleWordSize): LongSIBIndexReference =
    this.copy(
      displacement =
        Some(ImmediateValue.doubleWordImmediate(displacement.map(_.value).getOrElse(0) - newDisplacement.value)))
}

case class ProtectedSIBBaseIndexReference(
  baseRegister: GeneralPurposeRegister with ProtectedSIBBaseRegister with DoubleWordSize,
  index: Option[GeneralPurposeRegister with ProtectedSIBIndexRegister with DoubleWordSize],
  scale: Int = 1,
  displacement: Option[ImmediateValue[Int] with DoubleWordSize] = None,
  segment: SegmentRegister = Segment.Data
) extends ProtectedSIB {
  override val base: Option[GeneralPurposeRegister with ProtectedSIBBaseRegister with DoubleWordSize] = Some(
    baseRegister)
  final def +(newDisplacement: ImmediateValue[Int] with DoubleWordSize): ProtectedSIBBaseIndexReference =
    this.copy(
      displacement =
        Some(ImmediateValue.doubleWordImmediate(displacement.map(_.value).getOrElse(0) + newDisplacement.value)))

  final def -(newDisplacement: ImmediateValue[Int] with DoubleWordSize): ProtectedSIBBaseIndexReference =
    this.copy(
      displacement =
        Some(ImmediateValue.doubleWordImmediate(displacement.map(_.value).getOrElse(0) - newDisplacement.value)))
}

case class LongSIBBaseIndexReference(
  baseRegister: GeneralPurposeRegister with LongSIBBaseRegister with QuadWordSize,
  index: Option[GeneralPurposeRegister with LongSIBIndexRegister with QuadWordSize],
  scale: Int = 1,
  displacement: Option[ImmediateValue[Int] with DoubleWordSize] = None,
  segment: SegmentRegister = Segment.Data
) extends LongSIB {
  override val base: Option[GeneralPurposeRegister with LongSIBBaseRegister with QuadWordSize] = Some(baseRegister)
  final def +(newDisplacement: ImmediateValue[Int] with DoubleWordSize): LongSIBBaseIndexReference =
    this.copy(
      displacement =
        Some(ImmediateValue.doubleWordImmediate(displacement.map(_.value).getOrElse(0) + newDisplacement.value)))

  final def -(newDisplacement: ImmediateValue[Int] with DoubleWordSize): LongSIBBaseIndexReference =
    this.copy(
      displacement =
        Some(ImmediateValue.doubleWordImmediate(displacement.map(_.value).getOrElse(0) - newDisplacement.value)))
}

sealed case class ProtectedSIBBaseWithSegment(
  baseRegister: GeneralPurposeRegister with ProtectedSIBBaseRegister with DoubleWordSize,
  segment: SegmentRegister
) extends ProtectedSIB {
  override val base: Option[GeneralPurposeRegister with ProtectedSIBBaseRegister with DoubleWordSize] = Some(
    baseRegister)
  override val index: Option[GeneralPurposeRegister with ProtectedSIBIndexRegister with DoubleWordSize] = None
  override val scale: Int = 1
  override val displacement: Option[ImmediateValue[Int] with DoubleWordSize] = None

  final def +(sib: ProtectedSIB): ProtectedSIBBaseIndexReference =
    ProtectedSIBBaseIndexReference(baseRegister, sib.index, sib.scale, sib.displacement, segment)
}

sealed case class LongSIBBaseWithSegment(
  baseRegister: GeneralPurposeRegister with LongSIBBaseRegister with QuadWordSize,
  segment: SegmentRegister
) extends LongSIB {
  override val base: Option[GeneralPurposeRegister with LongSIBBaseRegister with QuadWordSize] = Some(baseRegister)
  override val index: Option[GeneralPurposeRegister with LongSIBIndexRegister with QuadWordSize] = None
  override val scale: Int = 1
  override val displacement: Option[ImmediateValue[Int] with DoubleWordSize] = None

  final def +(sib: LongSIB): LongSIBBaseIndexReference =
    LongSIBBaseIndexReference(baseRegister, sib.index, sib.scale, sib.displacement, segment)
}

sealed abstract class BaseIndexReference(
                                          val base: GeneralPurposeRegister with RealModeBaseRegister with WordSize,
                                          val index: GeneralPurposeRegister with CombinableRealModeIndexRegister with WordSize,
                                          override val indexCode: Byte)
  extends RegisterReference {

  override val defaultSegment: SegmentRegister = index.defaultSegment

  override def toString = s"$base+$index"
}

object BaseIndexReference {
  object BX_SI extends BaseIndexReference(Base.Word, SourceIndex.Real, 0x00)
  object BX_DI extends BaseIndexReference(Base.Word, DestinationIndex.Real, 0x01)
  object BP_SI extends BaseIndexReference(BasePointer.Real, SourceIndex.Real, 0x02)
  object BP_DI extends BaseIndexReference(BasePointer.Real, DestinationIndex.Real, 0x03)
}
