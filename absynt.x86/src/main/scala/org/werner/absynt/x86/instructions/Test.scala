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
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder._
import org.werner.absynt.x86.operations._
import org.werner.absynt.x86.{ArchitectureBounds, ProcessorMode}


object Test {

  sealed trait Common {
    self: ArchitectureBounds with OperandSizeInfo =>
    val mnemonic = "test"

    protected def Imm8ToAL(immediateValue: ImmediateValue[_] with ByteSize): X86Operation =
      new Static(0xA8.toByte :: Nil, mnemonic) with NoDisplacement with Immediate[ByteSize] {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Accumulator.LowByte, destination)

        override val immediateOrder: OperandOrder = source
        override val immediate: OperandWithOperandSizePrefixInfo[ImmediateValue[_] with ByteSize] = immediateValue
      }

    protected def Imm16ToAX(immediateValue: ImmediateValue[_] with WordSize): X86Operation =
      new Static(0xA9.toByte :: Nil, mnemonic) with NoDisplacement with Immediate[WordSize] {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Accumulator.Word, destination)

        override val immediateOrder: OperandOrder = source
        override val immediate: OperandWithOperandSizePrefixInfo[ImmediateValue[_] with WordSize] = immediateValue
      }

    protected def Imm32ToEAX(immediateValue: ImmediateValue[_] with DoubleWordSize): X86Operation =
      new Static(0xA9.toByte :: Nil, mnemonic) with NoDisplacement with Immediate[DoubleWordSize] {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Accumulator.DoubleWord, destination)

        override val immediateOrder: OperandOrder = source
        override val immediate: OperandWithOperandSizePrefixInfo[ImmediateValue[_] with DoubleWordSize] = immediateValue
      }

    protected def Imm32ToRAX(immediateValue: ImmediateValue[_] with DoubleWordSize): X86Operation =
      new Static(0xA9.toByte :: Nil, mnemonic) with NoDisplacement with Immediate[DoubleWordSize] {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Accumulator.QuadWord, destination)

        override val immediateOrder: OperandOrder = source
        override val immediate: OperandWithOperandSizePrefixInfo[ImmediateValue[_] with DoubleWordSize] = immediateValue
      }

    protected def Imm8ToRM8(operand: ModRMEncodableOperand with ByteSize, immediateValue: ImmediateValue[_] with ByteSize): X86Operation =
      new ModRM(operand, 0xF6.toByte :: Nil, 0x00.toByte, mnemonic, destination) with NoDisplacement with Immediate[ByteSize] {
        override val immediateOrder: OperandOrder = source
        override val immediate: OperandWithOperandSizePrefixInfo[ImmediateValue[_] with ByteSize] = immediateValue
      }

    protected def Imm16ToRM16[Size <: WordDoubleQuadSize](operand: ModRMEncodableOperand with Size, immediateValue: ImmediateValue[_] with Size): X86Operation =
      new ModRM(operand, 0xF7.toByte :: Nil, 0x00.toByte, mnemonic, destination) with NoDisplacement with Immediate[Size] {
        override val immediateOrder: OperandOrder = source
        override val immediate: OperandWithOperandSizePrefixInfo[ImmediateValue[_] with Size] = immediateValue
      }

    protected def R8ToRM8(operand1: ByteRegister, operand2: ModRMEncodableOperand with ByteSize): X86Operation =
      new ModRRM(operand1, operand2, 0x84.toByte :: Nil, mnemonic, destination)

    protected def R16ToRM16[Size <: WordDoubleQuadSize](operand1: GeneralPurposeRegister with Size, operand2: ModRMEncodableOperand with Size): X86Operation =
      new ModRRM(operand1, operand2, 0x85.toByte :: Nil, mnemonic, destination)

    sealed abstract class TestBase {
      def apply(source: ByteRegister, destination: ModRMEncodableOperand with ByteSize): X86Operation =
        R8ToRM8(source, destination)

      def apply(source: ByteRegister, destination: ByteRegister): X86Operation =
        R8ToRM8(source, destination)

      def apply[Size <: MaxWideSize](source: GeneralPurposeRegister with Size, destination: ModRMEncodableOperand with Size): X86Operation =
        R16ToRM16(source, destination)

      def apply[Size <: MaxWideSize](source: GeneralPurposeRegister with Size, destination: GeneralPurposeRegister with Size): X86Operation =
        R16ToRM16(source, destination)
    }
  }


  trait LegacyOperations extends Common {
    self: ProcessorMode.LegacyBounds with OperandSizeInfo  =>

    object Test extends TestBase  {

      def apply[Size <: ByteWordSize](immediate: ImmediateValue[_] with Size, destination: ModRMEncodableOperand with Size): X86Operation =
        (immediate, destination) match {
          case (imm: ImmediateValue[_] with ByteSize, d: ModRMEncodableOperand with ByteSize) =>
            Imm8ToRM8(d, imm)
          case (imm: ImmediateValue[_] with WordSize, d: ModRMEncodableOperand with WordSize) =>
            Imm16ToRM16(d, imm)
        }
    }
  }

  trait I386Operations extends Common {
    self: ProcessorMode.I386Bounds with OperandSizeInfo  =>

    object Test extends TestBase  {
      def apply(immediate: ImmediateValue[_] with ByteSize, destination: Accumulator.LowByte.type): X86Operation =
        Imm8ToAL(immediate)

      def apply(immediate: ImmediateValue[_] with WordSize, destination: Accumulator.Word.type): X86Operation =
        Imm16ToAX(immediate)

      def apply(immediate: ImmediateValue[_] with DoubleWordSize, destination: Accumulator.DoubleWord.type): X86Operation =
        Imm32ToEAX(immediate)

      def apply[Size <: ByteWordDoubleSize](immediate: ImmediateValue[_] with Size, destination: ModRMEncodableOperand with Size): X86Operation =
        (immediate, destination) match {
          case (imm: ImmediateValue[_] with ByteSize, d: ModRMEncodableOperand with ByteSize) =>
            Imm8ToRM8(d, imm)
          case (imm: ImmediateValue[_] with WordDoubleSize, d: ModRMEncodableOperand with WordDoubleSize) =>
            Imm16ToRM16(d, imm)
        }
    }
  }

  trait LongOperations extends Common {
    self: ProcessorMode.LongBounds with OperandSizeInfo  =>

    object Test extends TestBase {
      def apply(immediate: ImmediateValue[_] with ByteSize, destination: Accumulator.LowByte.type): X86Operation =
        Imm8ToAL(immediate)

      def apply(immediate: ImmediateValue[_] with WordSize, destination: Accumulator.Word.type): X86Operation =
        Imm16ToAX(immediate)

      def apply(immediate: ImmediateValue[_] with DoubleWordSize, destination: Accumulator.DoubleWord.type): X86Operation =
        Imm32ToEAX(immediate)

      def apply(immediate: ImmediateValue[_] with DoubleWordSize, destination: Accumulator.QuadWord.type): X86Operation =
        Imm32ToRAX(immediate)

      def apply[ImmediateSize <: ByteWordDoubleSize, DestinationSize <: ValueSize](immediate: ImmediateValue[_] with ImmediateSize, destination: ModRMEncodableOperand with DestinationSize): X86Operation =
        (immediate, destination) match {
          case (imm: ImmediateValue[_] with ByteSize, d: ModRMEncodableOperand with ByteSize) =>
            Imm8ToRM8(d, imm)
          case (imm: ImmediateValue[_] with DoubleWordSize, d: ModRMEncodableOperand with QuadWordSize) =>
            Imm16ToRM16(d, imm)
          case (imm: ImmediateValue[_] with WordDoubleSize, d: ModRMEncodableOperand with WordDoubleSize)
            if d sizeEquals imm =>
            Imm16ToRM16(d, imm)
          case _ =>
            throw new AssertionError
        }
    }
  }
}
