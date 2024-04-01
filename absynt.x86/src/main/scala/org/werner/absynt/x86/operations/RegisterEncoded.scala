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

import org.werner.absynt.x86.operands.{GeneralPurposeRegister, ValueSize}
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder.OperandOrder

abstract class RegisterEncoded[Size <: ValueSize](register: GeneralPurposeRegister & Size,
                                                              rawCode: Seq[Byte],
                                                              override val mnemonic: String)
                                                             (implicit operandSizePrefixRequirement: OperandSizePrefixRequirement)
  extends X86Operation(rawCode.take(rawCode.length - 1) :+ (rawCode.last | register.registerCode).toByte) with NoModRM {

  self: DisplacementBytes & ImmediateBytes =>
  def registerOrder: OperandOrder

  protected override def allOperands: Set[OperandInfo[?]] =
    super.allOperands + OperandInfo.encodedRegister(register, registerOrder)
}
