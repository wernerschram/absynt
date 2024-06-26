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

package org.werner.absynt.x86.instructions

import org.werner.absynt.x86.{ArchitectureBounds, ProcessorMode}
import org.werner.absynt.x86.operands.{ByteSize, ModRMEncodableOperand}
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder.*
import org.werner.absynt.x86.operations.*

object DivideMultiply {


  trait Operations {
    self: ArchitectureBounds & ProcessorMode =>


    private def RM8(operand: ModRMEncodableOperand & ByteSize, extensionCode: Byte, mnemonic: String): X86Operation =
      new ModRM(operand, 0xF6.toByte :: Nil, extensionCode, mnemonic, destination) with NoDisplacement with NoImmediate

    private def RM16[Size <: MaxWideSize](operand: ModRMEncodableOperand & Size, extensionCode: Byte, mnemonic: String): X86Operation =
      new ModRM(operand, 0xF7.toByte :: Nil, extensionCode, mnemonic, destination) with NoDisplacement with NoImmediate

    sealed class BasicDivideMultiply(extensionCode: Byte, val mnemonic: String){

      def apply[Size <: MaxValueSize](operand: ModRMEncodableOperand & Size): X86Operation =
        operand match {
          case o: ByteSize => RM8(o, extensionCode, mnemonic)
          case o: MaxWideSize @unchecked => RM16(o, extensionCode, mnemonic)
        }
    }

    object Divide extends BasicDivideMultiply(6.toByte, "div")
    object IntegerDivide extends BasicDivideMultiply(7.toByte, "idiv")

    object Multiply extends BasicDivideMultiply(4.toByte, "mul")
    object IntegerMultiply extends BasicDivideMultiply(5.toByte, "imul")
  }
}
