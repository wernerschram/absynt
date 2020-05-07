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

object String {
  trait Common[TS<:ValueSize, WS<:TS] {
    self: HasOperandSizePrefixRequirements =>

    val noOperandSizePrefixRequirements: OperandSizePrefixRequirement = new OperandSizePrefixRequirement {
      override def normalOperand(size: Operand with ValueSize): Boolean = false
      override def pointerOperand(size: Operand with FarPointerSize[_]): Boolean = false
    }

    implicit val noAddressSizePrefixRequirements: AddressSizePrefixRequirement = new AddressSizePrefixRequirement {
      override def normalAddress(size: Operand with ValueSize): Boolean = false
    }

    protected def Static8(operand: Byte, mnemonic: String, destination: DestinationReference with ByteSize): X86Operation =
      new Static(operand :: Nil, mnemonic) with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands +
            OperandInfo.implicitAddress(destination, OperandOrder.destination)(noOperandSizePrefixRequirements, noAddressSizePrefixRequirements) +
            OperandInfo.implicitOperand(Accumulator.LowByte, OperandOrder.source)(noOperandSizePrefixRequirements)
      }

    protected def Static16[Size <: WS](operand: Byte, mnemonic: String, register: AccumulatorRegister with Size, destination: DestinationReference with Size): X86Operation =
      new Static(operand :: Nil, mnemonic) with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands +
            OperandInfo.implicitAddress(destination, OperandOrder.destination)(noOperandSizePrefixRequirements, noAddressSizePrefixRequirements) +
            OperandInfo.implicitOperand(register, OperandOrder.source)(noOperandSizePrefixRequirements)
      }

    protected def RepStatic8(operand: Byte, mnemonic: String, destination: DestinationReference with ByteSize): X86Operation =
      new Static(operand :: Nil, mnemonic) with NoDisplacement with NoImmediate with Repeated {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands +
            OperandInfo.implicitAddress(destination, OperandOrder.destination)(noOperandSizePrefixRequirements, noAddressSizePrefixRequirements) +
            OperandInfo.implicitOperand(Accumulator.LowByte, OperandOrder.source)(noOperandSizePrefixRequirements)
      }

    protected def RepStatic16[Size <: WS](operand: Byte, mnemonic: String, register: AccumulatorRegister with Size, destination: DestinationReference with Size): X86Operation =
      new Static(operand :: Nil, mnemonic) with NoDisplacement with NoImmediate with Repeated {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands +
            OperandInfo.implicitAddress(destination, OperandOrder.destination)(noOperandSizePrefixRequirements, noAddressSizePrefixRequirements) +
            OperandInfo.implicitOperand(register, OperandOrder.source)(noOperandSizePrefixRequirements)
      }

    protected def RepEStatic8(operand: Byte, mnemonic: String, destination: DestinationReference with ByteSize): X86Operation =
      new Static(operand :: Nil, mnemonic) with NoDisplacement with NoImmediate with RepeatEqual {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands +
            OperandInfo.implicitAddress(destination, OperandOrder.destination)(noOperandSizePrefixRequirements, noAddressSizePrefixRequirements) +
            OperandInfo.implicitOperand(Accumulator.LowByte, OperandOrder.source)(noOperandSizePrefixRequirements)
      }

    protected def RepEStatic16[Size <: WS](operand: Byte, mnemonic: String, register: AccumulatorRegister with Size, destination: DestinationReference with Size): X86Operation =
      new Static(operand :: Nil, mnemonic) with NoDisplacement with NoImmediate with RepeatEqual {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands +
            OperandInfo.implicitAddress(destination, OperandOrder.destination)(noOperandSizePrefixRequirements, noAddressSizePrefixRequirements) +
            OperandInfo.implicitOperand(register, OperandOrder.source)(noOperandSizePrefixRequirements)
      }

    protected def RepNEStatic8(operand: Byte, mnemonic: String, destination: DestinationReference with ByteSize): X86Operation =
      new Static(operand :: Nil, mnemonic) with NoDisplacement with NoImmediate with RepeatNotEqual {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands +
            OperandInfo.implicitAddress(destination, OperandOrder.destination)(noOperandSizePrefixRequirements, noAddressSizePrefixRequirements) +
            OperandInfo.implicitOperand(Accumulator.LowByte, OperandOrder.source)(noOperandSizePrefixRequirements)
      }

    protected def RepNEStatic16[Size <: WS](operand: Byte, mnemonic: String, register: AccumulatorRegister with Size, destination: DestinationReference with Size): X86Operation =
      new Static(operand :: Nil, mnemonic) with NoDisplacement with NoImmediate with RepeatNotEqual {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands +
            OperandInfo.implicitAddress(destination, OperandOrder.destination)(noOperandSizePrefixRequirements, noAddressSizePrefixRequirements) +
            OperandInfo.implicitOperand(register, OperandOrder.source)(noOperandSizePrefixRequirements)
      }

    sealed class StringOperation(byteOperand: Byte, wideOperand: Byte, val mnemonic: String) {
      def apply[Size <: TS](register: AccumulatorRegister with Size, destination: DestinationReference with Size): X86Operation =
        (register, destination) match {
          case (Accumulator.LowByte, d: DestinationReference with ByteSize) => Static8(byteOperand, mnemonic, d)
          case (a: AccumulatorRegister with WS, d: DestinationReference with WS) => Static16(wideOperand, mnemonic, a, d)
        }

      object Repeat {
        def apply[Size <: TS](register: AccumulatorRegister with Size, destination: DestinationReference with Size): X86Operation =
          (register, destination) match {
            case (Accumulator.LowByte, d: DestinationReference with ByteSize) => RepStatic8(byteOperand, mnemonic, d)
            case (a: AccumulatorRegister with WS, d: DestinationReference with WS) => RepStatic16(wideOperand, mnemonic, a, d)
          }
      }
    }

    sealed class StringConditionOperation(byteOperand: Byte, wideOperand: Byte, mnemonic: String) {
      def apply[Size <: TS](register: AccumulatorRegister with Size, destination: DestinationReference with Size): X86Operation =
        (register, destination) match {
          case (Accumulator.LowByte, d: DestinationReference with ByteSize) => Static8(byteOperand, mnemonic, d)
          case (a: AccumulatorRegister with WS, d: DestinationReference with WS) => Static16(wideOperand, mnemonic, a, d)
        }

      object RepeatEqual {
        def apply[Size <: TS](register: AccumulatorRegister with Size, destination: DestinationReference with Size): X86Operation =
          (register, destination) match {
            case (Accumulator.LowByte, d: DestinationReference with ByteSize) => RepEStatic8(byteOperand, mnemonic, d)
            case (a: AccumulatorRegister with WS, d: DestinationReference with WS) => RepEStatic16(wideOperand, mnemonic, a, d)
          }
      }

      object RepeatNotEqual {
        def apply[Size <: TS](register: AccumulatorRegister with Size, destination: DestinationReference with Size): X86Operation =
          (register, destination) match {
            case (Accumulator.LowByte, d: DestinationReference with ByteSize) => RepNEStatic8(byteOperand, mnemonic, d)
            case (a: AccumulatorRegister with WS, d: DestinationReference with WS) => RepNEStatic16(wideOperand, mnemonic, a, d)
          }
      }
    }

    object InString extends StringOperation(0x6C.toByte, 0x6D.toByte, "ins")
    object MoveString extends StringOperation(0xA4.toByte, 0xA5.toByte, "movs")
    object OutString extends StringOperation(0x6E.toByte, 0x6F.toByte, "outs")
    object LoadString extends StringOperation(0xAC.toByte, 0xAD.toByte, "lods")
    object StoreString extends StringOperation(0xAA.toByte, 0xAB.toByte, "stos")
    object CompareString extends StringConditionOperation(0xA6.toByte, 0xA7.toByte, "cmps")
    object ScanString extends StringConditionOperation(0xAE.toByte, 0xAF.toByte, "scas")
  }

  trait LegacyOperations extends Common[ByteWordSize, WordSize] {
    self: HasOperandSizePrefixRequirements =>
  }

  trait RealOperations extends Common[ByteWordDoubleSize, WordDoubleSize] {
    self: HasOperandSizePrefixRequirements =>
  }

  trait ProtectedOperations extends Common[ByteWordDoubleSize, WordDoubleSize] {
    self: HasOperandSizePrefixRequirements =>
  }

  trait LongOperations extends Common[ValueSize, WordDoubleQuadSize] {
    self: HasOperandSizePrefixRequirements =>
  }
}