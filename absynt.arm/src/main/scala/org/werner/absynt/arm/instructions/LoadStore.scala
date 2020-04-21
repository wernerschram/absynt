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

import org.werner.absynt.arm.ProcessorMode
import org.werner.absynt.arm.operands.{ArmRelativeOffset, Condition}
import org.werner.absynt.arm.operands.registers.GeneralRegister
import org.werner.absynt.arm.operations.LoadStoreOperation.LoadStoreOperation
import org.werner.absynt.arm.operations._
import org.werner.absynt.resource.{RelativeReference, UnlabeledEncodable}
import org.werner.absynt.{Label, OffsetDirection, RelativeOffsetDirection}

abstract class LoadStoreReference(val opcode: String, targetLabel: Label, val condition: Condition)
  extends RelativeReference() with NamedConditional {
  override val target: Label = targetLabel

  override def sizeForDependencySize(distance: Int, offsetDirection: OffsetDirection): Int = 4

  override def possibleSizes: Set[Int] = Set(4)

  override def toString = s"$mnemonicString $target"
}

class LoadStoreRegister(
    wordOperation: LoadStoreOperation, byteOperation: LoadStoreOperation)(implicit val mnemonic: String) {

  private def ImmedWord(condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
                        offset: LoadStoreOffset, addressingType: LoadStoreAddressingType) =
    new LoadStore(mnemonic, condition, register, baseRegister, offset, addressingType, wordOperation)

  private def ImmedByte(condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
                        offset: LoadStoreOffset, addressingType: LoadStoreAddressingType) =
    new LoadStore(mnemonic, condition, register, baseRegister, offset, addressingType, byteOperation)

  def apply(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreOffset = LoadStoreOffset.noOffset,
            addressingType: LoadStoreAddressingTypeNormal = LoadStoreAddressingTypeNormal.OffsetNormal, condition: Condition = Condition.Always)
           (implicit processorMode: ProcessorMode): LoadStore =
    ImmedWord(condition, register, baseRegister, offset, addressingType)

  def byte(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreOffset = LoadStoreOffset.noOffset,
           addressingType: LoadStoreAddressingTypeNormal = LoadStoreAddressingTypeNormal.OffsetNormal, condition: Condition = Condition.Always)
          (implicit processorMode: ProcessorMode): LoadStore =
    ImmedByte(condition, register, baseRegister, offset, addressingType)

  def apply(targetLabel: Label, destination: GeneralRegister): RelativeReference =
    new LoadStoreReference(mnemonic, targetLabel, Condition.Always) {
      override def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): UnlabeledEncodable =
        ImmedWord(Condition.Always, destination, GeneralRegister.R15,
          LoadStoreOffset(ArmRelativeOffset.positionalOffset(distance)(offsetDirection).offset.toShort),
            LoadStoreAddressingTypeNormal.OffsetNormal)
    }

  def apply(targetLabel: Label, destination: GeneralRegister, condition: Condition): RelativeReference =
    new LoadStoreReference(mnemonic, targetLabel, condition) {
      override def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): UnlabeledEncodable =
        ImmedWord(condition, destination, GeneralRegister.R15,
          LoadStoreOffset(ArmRelativeOffset.positionalOffset(distance)(offsetDirection).offset.toShort),
            LoadStoreAddressingTypeNormal.OffsetNormal)
    }

  object UserMode {
    def apply(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreOffset, condition: Condition = Condition.Always)
             (implicit processorMode: ProcessorMode): LoadStore =
      ImmedWord(condition, register, baseRegister, offset, LoadStoreAddressingTypeUser.PostIndexedUser)

    def byte(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreOffset, condition: Condition = Condition.Always)
            (implicit processorMode: ProcessorMode): LoadStore =
      ImmedByte(condition, register, baseRegister, offset, LoadStoreAddressingTypeUser.PostIndexedUser)
  }
}
object LoadStore {

  trait A32Operations {

    object LoadRegister extends LoadStoreRegister(LoadStoreOperation.LoadWord, LoadStoreOperation.LoadByte)("ldr") {
      private def ImmedDoubleWord(condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
                                  offset: LoadStoreMiscellaneousOffset, addressingType: LoadStoreAddressingType) =
        new LoadStoreMiscelaneous("ldr", condition, register, baseRegister, offset, addressingType,
          LoadStoreMiscellaneousOperation.LoadDoubleWord)

      private def ImmedUnsignedHalfWord(condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
                                        offset: LoadStoreMiscellaneousOffset, addressingType: LoadStoreAddressingType) =
        new LoadStoreMiscelaneous("ldr", condition, register, baseRegister, offset, addressingType,
          LoadStoreMiscellaneousOperation.LoadUnsignedHalfWord)

      private def ImmedSignedByte(condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
                                  offset: LoadStoreMiscellaneousOffset, addressingType: LoadStoreAddressingType) =
        new LoadStoreMiscelaneous("ldr", condition, register, baseRegister, offset, addressingType,
          LoadStoreMiscellaneousOperation.LoadSignedByte)

      private def ImmedSignedHalfWord(condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
                                      offset: LoadStoreMiscellaneousOffset, addressingType: LoadStoreAddressingType) =
        new LoadStoreMiscelaneous("ldr", condition, register, baseRegister, offset, addressingType,
          LoadStoreMiscellaneousOperation.LoadSignedHalfWord)

      def doubleWord(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreMiscellaneousOffset,
                     addressingType: LoadStoreAddressingTypeNormal, condition: Condition = Condition.Always)
                    (implicit processorMode: ProcessorMode): LoadStoreMiscelaneous =
        ImmedDoubleWord(condition, register, baseRegister, offset, addressingType)

      def signedByte(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreMiscellaneousOffset,
                     addressingType: LoadStoreAddressingTypeNormal, condition: Condition = Condition.Always)
                    (implicit processorMode: ProcessorMode): LoadStoreMiscelaneous =
        ImmedSignedByte(condition, register, baseRegister, offset, addressingType)

      def unsignedHalfWord(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreMiscellaneousOffset,
                           addressingType: LoadStoreAddressingTypeNormal, condition: Condition = Condition.Always)
                          (implicit processorMode: ProcessorMode): LoadStoreMiscelaneous =
        ImmedUnsignedHalfWord(condition, register, baseRegister, offset, addressingType)

      def signedHalfWord(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreMiscellaneousOffset,
                         addressingType: LoadStoreAddressingTypeNormal, condition: Condition = Condition.Always)
                        (implicit processorMode: ProcessorMode): LoadStoreMiscelaneous =
        ImmedSignedHalfWord(condition, register, baseRegister, offset, addressingType)
    }

    object StoreRegister extends LoadStoreRegister(LoadStoreOperation.StoreWord, LoadStoreOperation.StoreByte)("str") {
      private def ImmedHalfWord(condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
                                offset: LoadStoreMiscellaneousOffset, addressingType: LoadStoreAddressingType) =
        new LoadStoreMiscelaneous("str", condition, register, baseRegister, offset, addressingType,
          LoadStoreMiscellaneousOperation.StoreHalfWord)

      private def ImmedDoubleWord(condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
                                  offset: LoadStoreMiscellaneousOffset, addressingType: LoadStoreAddressingType) =
        new LoadStoreMiscelaneous("str", condition, register, baseRegister, offset, addressingType,
          LoadStoreMiscellaneousOperation.StoreDoubleWord)

      def halfWord(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreMiscellaneousOffset,
                   addressingType: LoadStoreAddressingTypeNormal, condition: Condition = Condition.Always)
                  (implicit processorMode: ProcessorMode): LoadStoreMiscelaneous =
        ImmedHalfWord(condition, register, baseRegister, offset, addressingType)

      def doubleWord(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreMiscellaneousOffset,
                     addressingType: LoadStoreAddressingTypeNormal, condition: Condition = Condition.Always)
                    (implicit processorMode: ProcessorMode): LoadStoreMiscelaneous =
        ImmedDoubleWord(condition, register, baseRegister, offset, addressingType)
    }
  }
}
