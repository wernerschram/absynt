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

import org.werner.absynt.resource.UnlabeledEncodable
import org.werner.absynt.x86.RexRequirement
import org.werner.absynt.x86.operands._

abstract class X86Operation(val code: Seq[Byte]) extends UnlabeledEncodable {
  self: ModRMBytes & DisplacementBytes & ImmediateBytes =>
  final def prefixes: Seq[Byte] =
    optionalRepeatPrefix ++
      optionalSegmentOverridePrefix ++
      optionalAddressSizePrefix ++
      optionalOperandSizePrefix ++
      optionalRexPrefix

  protected def allOperands: Set[OperandInfo[?]] =
    Set.empty

  final lazy val operands: Set[OperandInfo[?]] = allOperands

  override def size: Int = encodeByte.length

  override def encodeByte: Seq[Byte] = {
    prefixes ++
      code ++
      modRMBytes ++
      displacementBytes ++
      immediateBytes
  }

  protected def optionalRepeatPrefix: List[Byte] =
    Nil

  private def optionalSegmentOverridePrefix: List[Byte] =
      operands.flatMap(_.addressOperands).flatMap(_.segmentOverride).flatMap(X86Operation.SegmentOverrideMap.get).toList

  private def optionalAddressSizePrefix: List[Byte] =
    if operands.flatMap(_.addressOperands).exists(_.requiresAddressSize) then X86Operation.AddressSizeCode :: Nil else Nil

  private def optionalOperandSizePrefix: List[Byte] =
    if operands.exists(_.requiresOperandSize) then X86Operation.OperandSizeCode :: Nil else Nil

  private def optionalRexPrefix: List[Byte] = {
    if rexRequirements.isEmpty then
      Nil
    else
      rexRequirements.foldLeft[Byte](X86Operation.RexCode)((value, req) => (value | req.rexBitMask).toByte) :: Nil
  }

  lazy val rexRequirements: Set[RexRequirement] =
    operands.flatMap(o => o.rexRequirements ++ o.addressOperands.flatMap(_.rexRequirements))

  def mnemonic: String

  val optionalRepeatPrefixString = ""

  override def toString: String = {
    val operandString = operands.toSeq.sorted.map(_.toString).mkString(", ")
    s"${optionalRepeatPrefixString}$mnemonic${if operandString.nonEmpty then s" $operandString" else ""}"
  }
}

object X86Operation {
  private val RexCode = 0x40.toByte

  private val OperandSizeCode = 0x66.toByte
  private val AddressSizeCode = 0x67.toByte

  private val SegmentOverrideMap: Map[SegmentRegister, Byte] = Map(
    (Segment.Code, 0x2E.toByte),
    (Segment.Stack, 0x36.toByte),
    (Segment.Data, 0x3E.toByte),
    (Segment.Extra, 0x26.toByte),
    (Segment.MoreExtra, 0x64.toByte),
    (Segment.StillMoreExtra, 0x65.toByte))
}
