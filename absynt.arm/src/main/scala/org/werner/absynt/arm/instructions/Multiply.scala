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
import org.werner.absynt.arm.operands.Condition.{Always, Condition}
import org.werner.absynt.arm.operands.registers.GeneralRegister
import org.werner.absynt.arm.operations.{MultiplyOperation, MultiplyWithRegisterOperation, SetFlags}

class MultiplyWithRegister(val code: Byte, val opcode: String) {

  def apply(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, addValue: GeneralRegister,
            condition: Condition = Always)(implicit processorMode: ProcessorMode): MultiplyWithRegisterOperation =
    RegRegAndRegToReg(destination, source, multiplyValue, addValue, condition)

  private def RegRegAndRegToReg(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister,
                                addValue: GeneralRegister, condition: Condition) =
    new MultiplyWithRegisterOperation(code, opcode, destination, source, multiplyValue, addValue, condition)

  def setFlags(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, addValue: GeneralRegister,
               condition: Condition = Always)(implicit processorMode: ProcessorMode): MultiplyWithRegisterOperation with SetFlags =
    RegRegAndRegToRegFlags(destination, source, multiplyValue, addValue, condition)

  private def RegRegAndRegToRegFlags(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister,
                                     addValue: GeneralRegister, condition: Condition) =
    new MultiplyWithRegisterOperation(code, opcode, destination, source, multiplyValue, addValue, condition) with SetFlags
}

class Multiply(val code: Byte, val opcode: String) {
  def apply(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, condition: Condition = Always)
           (implicit processorMode: ProcessorMode): MultiplyOperation =
    RegAndRegToReg(destination, source, multiplyValue, condition)

  private def RegAndRegToReg(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, condition: Condition) =
    new MultiplyOperation(code, opcode, destination, source, multiplyValue, condition)

  def setFlags(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, condition: Condition = Always)
              (implicit processorMode: ProcessorMode): MultiplyOperation with SetFlags =
    RegAndRegToRegFlags(destination, source, multiplyValue, condition)

  private def RegAndRegToRegFlags(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister,
                                  condition: Condition) =
    new MultiplyOperation(code, opcode, destination, source, multiplyValue, condition) with SetFlags
}


object Multiply {
  trait A32Operations {
    object MultiplyAccumulate extends MultiplyWithRegister(0x01.toByte, "mla")

    object Multiply extends Multiply(0x00.toByte, "mul")
  }
}
