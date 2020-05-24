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
import org.werner.absynt.x86.operands.registers.SegmentRegister
import org.werner.absynt.x86.operations.{AddressOperandInfo, AddressSizePrefixRequirement}

abstract class MemoryLocation(val displacement: Option[ImmediateValue[_]], val segment: SegmentRegister)
  extends ModRMEncodableOperand {

  def addressOperands(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): Set[AddressOperandInfo]

  def defaultSegment: SegmentRegister
  lazy val segmentOverride: Option[SegmentRegister] = if (segment == defaultSegment) None else Some(segment)

  def segmentPrefix: String = segmentOverride match {
    case Some(segmentRegister) => s"$segmentRegister:"
    case None => ""
  }
}

