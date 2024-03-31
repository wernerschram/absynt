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

package org.werner.absynt.arm.instructions

import org.werner.absynt.arm.operands.{ArmRelativeOffset, Condition, RelativeA32Pointer, RelativePointer, RelativeThumbPointer}
import org.werner.absynt.arm.operands.registers.GeneralRegister
import org.werner.absynt.arm.operations.{BranchImmediate, BranchRegister, NamedConditional}
import org.werner.absynt.resource.{RelativeReference, UnlabeledEncodable}
import org.werner.absynt.{Label, OffsetDirection, RelativeOffsetDirection}

abstract class BranchReference(val opcode: String, targetLabel: Label, val condition: Condition)
  extends RelativeReference() with NamedConditional {
  override val target: Label = targetLabel

  override def sizeForDependencySize(distance: Int, offsetDirection: OffsetDirection): Int = 4

  override def possibleSizes: Set[Int] = Set(4)

  override def toString = s"$mnemonicString $target"
}

class Branch(code: Byte, val opcode: String) {
  def apply(destination: RelativeA32Pointer, condition: Condition = Condition.Always):  BranchImmediate[RelativeA32Pointer] =
    Immediate(destination, condition)

  def apply(targetLabel: Label): BranchReference =
    new BranchReference(opcode, targetLabel, Condition.Always) {
      override def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): UnlabeledEncodable =
        Immediate(RelativeA32Pointer(ArmRelativeOffset.positionalOffset(distance)(offsetDirection)), Condition.Always)
    }

  private def Immediate[AddressType<:RelativePointer](destination: AddressType, condition: Condition = Condition.Always) =
    new BranchImmediate[AddressType](destination, condition, code, opcode)

  def apply(targetLabel: Label, condition: Condition): RelativeReference =
    new BranchReference(opcode, targetLabel, condition) {
      override def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): UnlabeledEncodable =
        Immediate(RelativeA32Pointer(ArmRelativeOffset.positionalOffset(distance)(offsetDirection)), this.condition)
    }
}

class BranchExchange(registerCode: Byte, val opcode: String) {
  def apply(destination: GeneralRegister, condition: Condition = Condition.Always): BranchRegister =
    Register(destination, condition)

  private def Register(destination: GeneralRegister, condition: Condition = Condition.Always) =
    new BranchRegister(destination, condition, registerCode, opcode)
}

class BranchLinkExchange(immediateCode: Byte, registerCode: Byte, opcode: String) extends BranchExchange(registerCode, opcode) {
  def apply(destination: RelativeThumbPointer): BranchImmediate[RelativeThumbPointer] =
    Immediate(destination, Condition.Unpredictable)

  private def Immediate(destination: RelativeThumbPointer, condition: Condition = Condition.Always) =
    new BranchImmediate(destination, condition, immediateCode, opcode)

  def apply(targetLabel: Label): BranchReference =
    new BranchReference(opcode, targetLabel, Condition.Unpredictable) {
      override def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): UnlabeledEncodable =
        Immediate(RelativeThumbPointer(ArmRelativeOffset.positionalOffset(distance)(offsetDirection)), Condition.Unpredictable)
    }
}

object Branch {
  trait A32Operations {
    object Branch extends Branch(0xA0.toByte, "b")

    object BranchLink extends Branch(0xB0.toByte, "bl")

    object BranchExchange extends BranchExchange(0x1.toByte, "bx")

    object BranchLinkExchange extends BranchLinkExchange(0xA0.toByte, 0x3.toByte, "blx")

    object BranchExchangeJazelle extends BranchExchange(0x2.toByte, "bxj")
  }
}

