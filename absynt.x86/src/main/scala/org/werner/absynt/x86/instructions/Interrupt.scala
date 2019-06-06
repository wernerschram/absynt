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
import org.werner.absynt.x86.operands.{ByteSize, ImmediateValue}
import org.werner.absynt.x86.operations._
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder._

object Interrupt {


  trait Operations {
    self: HasOperandSizePrefixRequirements =>

    private def Static(mnemonic: String)(implicit byteImmediate: ValueToByteImmediate) =
      new Static(0xCC.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(byteImmediate(3.toByte), destination)
      }

    private def Imm8(immediateValue: ImmediateValue with ByteSize, mnemonic: String) =
      new Static(0xCD.toByte :: Nil, mnemonic) with NoDisplacement with Immediate[ByteSize] with HasOperandSizePrefixRequirements {
        override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = Operations.this.operandSizePrefixRequirement

        override def immediate: ImmediateValue with ByteSize = immediateValue

        override def immediateOrder: OperandOrder = destination
      }

    object Interrupt {
      val mnemonic: String = "int"

      def apply(immediate: ImmediateValue with ByteSize)(implicit byteImmediate: ValueToByteImmediate): Static = immediate.value.head match {
        case 3 => Static(mnemonic)
        case _ => Imm8(immediate, mnemonic)
      }
    }

    object ClearInterruptFlag {
      val mnemonic: String = "cli"

      def apply(): Static =
        new Static(0xFA.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate
    }

    object SetInterruptFlag {
      val mnemonic: String = "sti"

      def apply(): Static =
        new Static(0xFB.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate
    }

  }
}
