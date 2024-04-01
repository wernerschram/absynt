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

import org.werner.absynt.x86.operands._
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder.{OperandOrder, destination}
import org.werner.absynt.x86.operations._
import org.werner.absynt.x86.{ArchitectureBounds, ProcessorMode}

object IncrementDecrement {

  sealed trait Common {
    self: ArchitectureBounds & ProcessorMode =>

    private def RM8(operand: ModRMEncodableOperand & ByteSize, rValue: Byte, mnemonic: String): X86Operation =
      new ModRM(operand, 0xFE.toByte :: Nil, rValue, mnemonic, destination) with NoDisplacement with NoImmediate

    private def RM16[Size <: MaxWideSize](operand: ModRMEncodableOperand & Size, rValue: Byte, mnemonic: String): X86Operation =
      new ModRM(operand, 0xFF.toByte :: Nil, rValue, mnemonic, destination) with NoDisplacement with NoImmediate

    abstract class BaseOperation(extension: Byte, val mnemonic: String) {

      def apply[Size <: MaxValueSize](destination: ModRMEncodableOperand & Size): X86Operation = destination match {
        case d: ByteSize =>
          RM8(d, extension, mnemonic)
        case d: MaxWideSize @unchecked =>
          RM16(d, extension, mnemonic)
      }
    }
  }

  sealed trait Shorter extends Common {
    self: ArchitectureBounds & ProcessorMode =>

    private def R16[Size <: MaxWideSize](register: GeneralPurposeRegister & Size, opcodeBase: Byte, mnemonic: String) =
      new RegisterEncoded[Size](register, Seq(opcodeBase), mnemonic) with NoDisplacement with NoImmediate {
      override def registerOrder: OperandOrder = destination
    }

    trait ShorterOperation {
      self: BaseOperation =>
      val shortOpcodeBase: Byte

      def apply[Size <: MaxWideSize](destination: GeneralPurposeRegister & Size): X86Operation =
        R16(destination, shortOpcodeBase, mnemonic)
    }

    object Increment extends BaseOperation(0, "inc") with ShorterOperation {
      override val shortOpcodeBase: Byte = 0x40
    }

    object Decrement extends BaseOperation(1, "dec") with ShorterOperation {
      override val shortOpcodeBase: Byte = 0x48
    }
  }

  sealed trait NoShorter extends Common {
    self: ArchitectureBounds & ProcessorMode =>

    object Increment extends BaseOperation(0, "inc")
    object Decrement extends BaseOperation(1, "dec")
  }

  trait LegacyOperations extends Shorter {
    self: ProcessorMode.LegacyBounds & ProcessorMode =>
  }

  trait I386Operations extends Shorter {
    self: ProcessorMode.I386Bounds & ProcessorMode =>
  }

  trait LongOperations extends NoShorter {
    self: ProcessorMode.LongBounds & ProcessorMode =>
  }
}
