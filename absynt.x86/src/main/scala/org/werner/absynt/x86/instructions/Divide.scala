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

import org.werner.absynt.x86.HasOperandSizePrefixRequirements
import org.werner.absynt.x86.operands.ImmediateValue.ValueToByteImmediate
import org.werner.absynt.x86.operands.{ByteSize, ImmediateValue, ModRMEncodableOperand, ValueSize, WordDoubleQuadSize}
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder._
import org.werner.absynt.x86.operations._

object Divide {


  trait Operations {
    self: HasOperandSizePrefixRequirements =>


    private def RM8(operand: ModRMEncodableOperand with ByteSize, mnemonic: String): X86Operation =
      new ModRM(operand, 0xF6.toByte :: Nil, 6, mnemonic, destination) with NoDisplacement with NoImmediate

    private def RM16[Size <: WordDoubleQuadSize](operand: ModRMEncodableOperand with Size, mnemonic: String): X86Operation =
      new ModRM(operand, 0xF7.toByte :: Nil, 6, mnemonic, destination) with NoDisplacement with NoImmediate

    object Divide{
      val mnemonic: String = "div"

      def apply(operand: ModRMEncodableOperand with ValueSize): X86Operation =
        operand match {
          case o: ByteSize => RM8(o, mnemonic)
          case o: WordDoubleQuadSize => RM16(o, mnemonic)
        }
    }
  }
}
