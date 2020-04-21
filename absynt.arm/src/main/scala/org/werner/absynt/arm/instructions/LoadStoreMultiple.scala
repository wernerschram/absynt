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
import org.werner.absynt.arm.operands.Condition
import org.werner.absynt.arm.operands.registers.GeneralRegister
import org.werner.absynt.arm.operations.{LoadStoreMultiple => LoadStoreMultipleOperation, LoadStoreMultipleDirection, UpdateBase, UpdateMode, UserModeRegisters, ReturnFromException => ReturnFromExceptionOperation}

object LoadStoreMultiple {

  trait A32Operations {
    self: Condition.ARMCondition =>
    object LoadMultiple {
      implicit val opcode: String = "ldm"

      def apply(registers: Seq[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode, condition: Condition = Condition.Always)
               (implicit processorMode: ProcessorMode): LoadStoreMultipleOperation =
        Immed(condition, registers, baseRegister, addressingMode)

      private def Immed(condition: Condition, registers: Seq[GeneralRegister], baseRegister: GeneralRegister,
                        addressingMode: UpdateMode) =
        new LoadStoreMultipleOperation(LoadStoreMultipleDirection.Load, condition, registers, baseRegister, addressingMode, opcode)

      def withUpdateBase(registers: Seq[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode,
                         condition: Condition = Condition.Always)(implicit processorMode: ProcessorMode): LoadStoreMultipleOperation with UpdateBase =
        ImmedUpdateBase(condition, registers, baseRegister, addressingMode)

      private def ImmedUpdateBase(condition: Condition, registers: Seq[GeneralRegister], baseRegister: GeneralRegister,
                                  addressingMode: UpdateMode) =
        new LoadStoreMultipleOperation(LoadStoreMultipleDirection.Load, condition, registers, baseRegister, addressingMode, opcode) with UpdateBase

      def withUserModeRegisters(registers: Seq[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode,
                                condition: Condition = Condition.Always)(implicit processorMode: ProcessorMode): LoadStoreMultipleOperation with UserModeRegisters =
        ImmedUserModeRegisters(condition, registers, baseRegister, addressingMode)

      private def ImmedUserModeRegisters(condition: Condition, registers: Seq[GeneralRegister], baseRegister: GeneralRegister,
                                         addressingMode: UpdateMode) =
        new LoadStoreMultipleOperation(LoadStoreMultipleDirection.Load, condition, registers, baseRegister, addressingMode, opcode)
          with UserModeRegisters

      def withUserModeRegistersAndUpdateBase(registers: Seq[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode,
                                             condition: Condition = Condition.Always)(implicit processorMode: ProcessorMode): LoadStoreMultipleOperation with UpdateBase with UserModeRegisters =
        ImmedUserModeRegistersAndUpdateBase(condition, registers, baseRegister, addressingMode)

      private def ImmedUserModeRegistersAndUpdateBase(condition: Condition, registers: Seq[GeneralRegister],
                                                      baseRegister: GeneralRegister, addressingMode: UpdateMode) =
        new LoadStoreMultipleOperation(LoadStoreMultipleDirection.Load, condition, registers, baseRegister, addressingMode, opcode)
          with UpdateBase with UserModeRegisters
    }

    object StoreMultiple {
      implicit val opcode: String = "stm"

      def apply(registers: Seq[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode, condition: Condition = Condition.Always)
               (implicit processorMode: ProcessorMode): LoadStoreMultipleOperation =
        Immed(condition, registers, baseRegister, addressingMode)

      private def Immed(condition: Condition, registers: Seq[GeneralRegister], baseRegister: GeneralRegister,
                        addressingMode: UpdateMode) =
        new LoadStoreMultipleOperation(LoadStoreMultipleDirection.Store, condition, registers, baseRegister, addressingMode, opcode)

      def withUpdateBase(registers: Seq[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode,
                         condition: Condition = Condition.Always)(implicit processorMode: ProcessorMode): LoadStoreMultipleOperation with UpdateBase =
        ImmedUpdateBase(condition, registers, baseRegister, addressingMode)

      private def ImmedUpdateBase(condition: Condition, registers: Seq[GeneralRegister], baseRegister: GeneralRegister,
                                  addressingMode: UpdateMode) =
        new LoadStoreMultipleOperation(LoadStoreMultipleDirection.Store, condition, registers, baseRegister, addressingMode, opcode)
          with UpdateBase

      def withUserModeRegisters(registers: Seq[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode,
                                condition: Condition = Condition.Always)(implicit processorMode: ProcessorMode): LoadStoreMultipleOperation with UserModeRegisters =
        ImmedUserModeRegisters(condition, registers, baseRegister, addressingMode)

      private def ImmedUserModeRegisters(condition: Condition, registers: Seq[GeneralRegister], baseRegister: GeneralRegister,
                                         addressingMode: UpdateMode) =
        new LoadStoreMultipleOperation(LoadStoreMultipleDirection.Store, condition, registers, baseRegister, addressingMode, opcode)
          with UserModeRegisters
    }

    object ReturnFromException {
      implicit val opcode: String = "rfe"

      def apply(baseRegister: GeneralRegister, addressingMode: UpdateMode)(implicit processorMode: ProcessorMode): ReturnFromExceptionOperation =
        Immed(baseRegister, addressingMode, updateBase = false)

      private def Immed(baseRegister: GeneralRegister, addressingMode: UpdateMode, updateBase: Boolean) =
        new ReturnFromExceptionOperation(baseRegister, addressingMode, updateBase, opcode)

      def withUpdateBase(baseRegister: GeneralRegister, addressingMode: UpdateMode)(implicit processorMode: ProcessorMode): ReturnFromExceptionOperation =
        Immed(baseRegister, addressingMode, updateBase = true)
    }

    object Push {
      def apply(registers: Seq[GeneralRegister])(implicit processorMode: ProcessorMode): LoadStoreMultipleOperation with UpdateBase =
        LoadMultiple.withUpdateBase(registers, GeneralRegister.R13, UpdateMode.DecrementAfter)
    }

    object Pop {
      def apply(registers: Seq[GeneralRegister])(implicit processorMode: ProcessorMode): LoadStoreMultipleOperation with UpdateBase =
        StoreMultiple.withUpdateBase(registers, GeneralRegister.R13, UpdateMode.IncrementBefore)
    }

  }
}
