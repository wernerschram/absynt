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
import org.werner.absynt.arm.operands.Condition._
import org.werner.absynt.arm.operands.RightRotateImmediate
import org.werner.absynt.arm.operands.registers._
import org.werner.absynt.arm.operations.{Fields, MoveFromStatusRegister => MoveFromStatusRegisterOperation, MoveToStatusRegister => MoveToStatusRegisterOpcode}

object MoveStatusRegister {

  trait A32Operations {
    object MoveFromStatusRegister {
      implicit val opcode: String = "mrs"

      def apply(source: StatusRegister, destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode): MoveFromStatusRegisterOperation =
        RegToStatus(source, destination, condition)

      private def RegToStatus(source: StatusRegister, destination: GeneralRegister, condition: Condition) =
        new MoveFromStatusRegisterOperation(opcode, source, destination, condition)
    }

    object MoveToStatusRegister {
      implicit val opcode: String = "msr"

      def apply(source: GeneralRegister, destination: StatusRegister, fields: Fields.ValueSet)(implicit processorMode: ProcessorMode): MoveToStatusRegisterOpcode =
        RegToReg(source, destination, fields, Always)

      def apply(source: GeneralRegister, destination: StatusRegister, fields: Fields.ValueSet, condition: Condition)
               (implicit processorMode: ProcessorMode): MoveToStatusRegisterOpcode =
        RegToReg(source, destination, fields, condition)

      private def RegToReg(source: GeneralRegister, destination: StatusRegister, fields: Fields.ValueSet, condition: Condition) =
        new MoveToStatusRegisterOpcode(opcode, source, destination, fields, condition)

      def apply(source: RightRotateImmediate, destination: StatusRegister, fields: Fields.ValueSet)(implicit processorMode: ProcessorMode): MoveToStatusRegisterOpcode =
        ImmediateToReg(source, destination, fields, Always)

      def apply(source: RightRotateImmediate, destination: StatusRegister, fields: Fields.ValueSet, condition: Condition)
               (implicit processorMode: ProcessorMode): MoveToStatusRegisterOpcode =
        ImmediateToReg(source, destination, fields, condition)

      private def ImmediateToReg(source: RightRotateImmediate, destination: StatusRegister, fields: Fields.ValueSet, condition: Condition) =
        new MoveToStatusRegisterOpcode(opcode, source, destination, fields, condition)
    }
  }
}
