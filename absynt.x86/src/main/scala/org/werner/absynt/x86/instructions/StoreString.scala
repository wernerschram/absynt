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
import org.werner.absynt.x86.operands._
import org.werner.absynt.x86.operands.memoryaccess.DestinationReference
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder
import org.werner.absynt.x86.operations._

object StoreString {
  implicit val mnemonic: String = "stos"

  trait Common {
    self: HasOperandSizePrefixRequirements =>
    protected def Static8(destination: DestinationReference with ByteSize): X86Operation =
      new Static(0xAA.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands +
            OperandInfo.implicitAddress(destination, OperandOrder.destination) +
            OperandInfo.implicitOperand(Accumulator.LowByte, OperandOrder.source)
      }

    protected def Static16[Size <: WordDoubleQuadSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size): X86Operation =
      new Static(0xAB.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands +
            OperandInfo.implicitAddress(destination, OperandOrder.destination) +
            OperandInfo.implicitOperand(register, OperandOrder.source)
      }

    protected def RepStatic8(destination: DestinationReference with ByteSize): X86Operation =
      new Static(0xAA.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate with Repeated {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands +
            OperandInfo.implicitAddress(destination, OperandOrder.destination) +
            OperandInfo.implicitOperand(Accumulator.LowByte, OperandOrder.source)
      }

    protected def RepStatic16[Size <: WordDoubleQuadSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size): X86Operation =
      new Static(0xAB.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate with Repeated {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands +
            OperandInfo.implicitAddress(destination, OperandOrder.destination) +
            OperandInfo.implicitOperand(register, OperandOrder.source)
      }
  }

  trait LegacyOperations extends Common {
    self: HasOperandSizePrefixRequirements =>
    object StoreString {
      def apply[Size <: ByteWordSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size): X86Operation =
        (register, destination) match {
          case (Accumulator.LowByte, d: DestinationReference with ByteSize) => Static8(d)
          case (a: AccumulatorRegister with WordSize, d: DestinationReference with WordSize) => Static16(a, d)
        }

      object Repeat {
        def apply[Size <: ByteWordSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size): X86Operation =
          (register, destination) match {
            case (Accumulator.LowByte, d: DestinationReference with ByteSize) => RepStatic8(d)
            case (a: AccumulatorRegister with WordSize, d: DestinationReference with WordSize) => RepStatic16(a, d)
          }
      }
    }
  }

  trait RealOperations extends Common {
    self: HasOperandSizePrefixRequirements =>
    object StoreString {
      def apply[Size <: ByteWordDoubleSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size): X86Operation =
        (register, destination) match {
          case (Accumulator.LowByte, d: DestinationReference with ByteSize) => Static8(d)
          case (a: AccumulatorRegister with WordDoubleSize, d: DestinationReference with WordDoubleSize) => Static16(a, d)
        }

      object Repeat {
        def apply[Size <: ByteWordDoubleSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size): X86Operation =
          (register, destination) match {
            case (Accumulator.LowByte, d: DestinationReference with ByteSize) => RepStatic8(d)
            case (a: AccumulatorRegister with WordSize, d: DestinationReference with WordSize) => RepStatic16(a, d)
          }
      }
    }
  }

  trait ProtectedOperations extends Common {
    self: HasOperandSizePrefixRequirements =>
    object StoreString {
      def apply[Size <: ByteWordDoubleSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size): X86Operation =
        (register, destination) match {
          case (Accumulator.LowByte, d: DestinationReference with ByteSize) => Static8(d)
          case (a: AccumulatorRegister with WordDoubleSize, d: DestinationReference with WordDoubleSize) => Static16(a, d)
        }

      object Repeat {
        def apply[Size <: ByteWordDoubleSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size): X86Operation =
          (register, destination) match {
            case (Accumulator.LowByte, d: DestinationReference with ByteSize) => RepStatic8(d)
            case (a: AccumulatorRegister with WordSize, d: DestinationReference with WordSize) => RepStatic16(a, d)
          }
      }
    }
  }

  trait LongOperations extends Common {
    self: HasOperandSizePrefixRequirements =>
    object StoreString {
      def apply[Size <: ValueSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size): X86Operation =
        (register, destination) match {
          case (Accumulator.LowByte, d: DestinationReference with ByteSize) => Static8(d)
          case (a: AccumulatorRegister with WordDoubleQuadSize, d: DestinationReference with WordDoubleQuadSize) => Static16(a, d)
        }

      object Repeat {
        def apply[Size <: ValueSize](register: AccumulatorRegister with Size, destination: DestinationReference with Size): X86Operation =
          (register, destination) match {
            case (Accumulator.LowByte, d: DestinationReference with ByteSize) => RepStatic8(d)
            case (a: AccumulatorRegister with WordSize, d: DestinationReference with WordSize) => RepStatic16(a, d)
          }
      }
    }
  }
}