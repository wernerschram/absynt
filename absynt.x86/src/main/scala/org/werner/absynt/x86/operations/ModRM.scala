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

import org.werner.absynt.x86.operands.*
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder.{OperandOrder, *}

sealed trait ModRMBytes {
  def modRMBytes: Seq[Byte]
}

trait NoModRM extends ModRMBytes {
  self: X86Operation & DisplacementBytes & ImmediateBytes =>
  override def modRMBytes: Seq[Byte] = Nil
}

// TODO: There should also be an option to create a disp32 ModRM: a displacement without operand (the value at an absolute address)
abstract class ModRM[Size<:ValueSize](val operandRM: ModRMEncodableOperand & Size,
                     override val code: Seq[Byte],
                     val rValue: Byte,
                     override val mnemonic: String,
                     val operandRMOrder: OperandOrder,
                     includeRexW: Boolean = true)(using OperandSizePrefixRequirement, AddressSizePrefixRequirement)
  extends X86Operation(code), ModRMBytes {

  self: DisplacementBytes & ImmediateBytes =>

  override def modRMBytes: Seq[Byte] = operandRM.getExtendedBytes(rValue)

  protected override def allOperands: Set[OperandInfo[?]] =
    super.allOperands + OperandInfo.rmRegisterOrMemory(operandRM, operandRMOrder, includeRexW)
}

class ModRRM[Size <: ValueSize](val register: GeneralPurposeRegister & Size,
                                operandRM: ModRMEncodableOperand & Size,
                                override val code: Seq[Byte],
                                override val mnemonic: String,
                                override val operandRMOrder: OperandOrder)
                               (using OperandSizePrefixRequirement, AddressSizePrefixRequirement)
  extends ModRM(operandRM, code, register.registerOrMemoryModeCode, mnemonic, operandRMOrder), NoDisplacement, NoImmediate {

  def operandROrder: OperandOrder =
    if operandRMOrder == destination then source else destination

  protected override def allOperands: Set[OperandInfo[?]] =
    super.allOperands + OperandInfo.rmRegister(register, operandROrder)
}

class ModSegmentRM[Size <: WordSize | DoubleWordSize | QuadWordSize](val register: SegmentRegister,
                                                                    operandRM: ModRMEncodableOperand & Size,
                                                                    override val code: Seq[Byte],
                                                                    override val mnemonic: String,
                                                                    override val operandRMOrder: OperandOrder)
                                                                   (using OperandSizePrefixRequirement, AddressSizePrefixRequirement)
  extends ModRM(operandRM, code, register.registerCode, mnemonic, operandRMOrder) {
  self: X86Operation & DisplacementBytes & ImmediateBytes =>

  def operandSegmentOrder: OperandOrder =
    if operandRMOrder == destination then source else destination

  protected override def allOperands: Set[OperandInfo[?]] =
    super.allOperands + OperandInfo.rmSegment(register, operandSegmentOrder)
}
