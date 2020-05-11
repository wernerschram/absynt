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

package org.werner.absynt.x86.operations

import org.werner.absynt.x86.HasOperandSizePrefixRequirements
import org.werner.absynt.x86.operands.{ImmediateValue, ValueSize}
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder._

sealed trait ImmediateBytes {
  self: X86Operation =>
  def immediateBytes: Seq[Byte]
  protected override def allOperands: Set[OperandInfo[_]]
}

trait NoImmediate extends ImmediateBytes {
  self: X86Operation =>
  override def immediateBytes: Seq[Byte] = Nil
}

trait Immediate[Size<:ValueSize] extends ImmediateBytes {

  // TODO: remove this selftype as HasOperandPrefixRequirements application is unclear at this point
  self: X86Operation with HasOperandSizePrefixRequirements =>

  def immediate: ImmediateValue with Size
  def immediateOrder: OperandOrder

  protected override abstract def allOperands: Set[OperandInfo[_]] =
    super.allOperands + OperandInfo.immediate(immediate, immediateOrder)

  override def immediateBytes: Seq[Byte] = immediate.value
}
