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

import org.werner.absynt.x86.operands.{ModRMEncodableOperand, WordSize}

sealed trait RMRegisterReference {
  val defaultSegment: SegmentRegister = Segment.Data
  val indexCode: Byte

  def onlyWithDisplacement: Boolean = false
}

sealed trait RMIndexRegister extends RMRegisterReference {
  self: GeneralPurposeRegister =>
}

trait RealRMIndexRegister extends RMIndexRegister {
  self: GeneralPurposeRegister =>
}

trait CombinableRealRMIndexRegister extends RealRMIndexRegister {
  self: GeneralPurposeRegister =>

  def +(base: RealRMBaseRegister): BaseIndexReference =
    base.combinedIndex(this)
}

trait RealRMBaseRegister extends ModRMEncodableOperand {
  self: GeneralPurposeRegister =>

  def combinedIndex(index: CombinableRealRMIndexRegister): BaseIndexReference

  final def +(index: CombinableRealRMIndexRegister): BaseIndexReference =
    combinedIndex(index)
}

trait ProtectedRMIndexRegister extends RMIndexRegister {
  self: GeneralPurposeRegister =>
  override val indexCode: Byte = self.registerOrMemoryModeCode
}

sealed abstract class BaseIndexReference(
  val base: GeneralPurposeRegister with RealRMBaseRegister with WordSize,
  val index: GeneralPurposeRegister with CombinableRealRMIndexRegister with WordSize,
  override val indexCode: Byte)
extends RMRegisterReference {

  override val defaultSegment: SegmentRegister = index.defaultSegment

  override def toString = s"$base+$index"
}

object BaseIndexReference {
  object BX_SI extends BaseIndexReference(Base.Word, SourceIndex.Real, 0x00)
  object BX_DI extends BaseIndexReference(Base.Word, DestinationIndex.Real, 0x01)
  object BP_SI extends BaseIndexReference(BasePointer.Real, SourceIndex.Real, 0x02)
  object BP_DI extends BaseIndexReference(BasePointer.Real, DestinationIndex.Real, 0x03)
}
