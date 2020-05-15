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

import org.werner.absynt._
import org.werner.absynt.resource.{AbsoluteReference, UnlabeledEncodable}
import org.werner.absynt.x86.operands.Register.I8086GenericRegisters
import org.werner.absynt.x86.operands._
import org.werner.absynt.x86.operands.memoryaccess._
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder._
import org.werner.absynt.x86.operations.{Immediate, ModRM, ModRRM, ModSegmentRM, NoDisplacement, NoImmediate, OperandInfo, OperandSizeInfo, OperandWithOperandSizePrefixInfo, OperandWithSizePrefixInfo, RegisterEncoded, Static, X86Operation, MemoryLocation => MemoryLocationOperation}
import org.werner.absynt.x86._

object Move extends I8086GenericRegisters {

  implicit val mnemonic: String = "mov"

  sealed trait Common {
    self: ArchitectureBounds with OperandSizeInfo  =>

    trait BasicMove {

      protected def RM16ToSReg[Size <: MaxWideSize](operand1: SegmentRegister, operand2: ModRMEncodableOperand with Size) =
        new ModSegmentRM(operand1, operand2, 0x8E.toByte :: Nil, mnemonic, source) with NoDisplacement with NoImmediate

      protected def SRegToRM16[Size <: MaxWideSize](operand1: SegmentRegister, operand2: ModRMEncodableOperand with Size) =
        new ModSegmentRM(operand1, operand2, 0x8C.toByte :: Nil, mnemonic, destination) with NoDisplacement with NoImmediate

      protected def R8ToRM8(operand1: ByteRegister, operand2: ModRMEncodableOperand with ByteSize) =
        new ModRRM(operand1, operand2, 0x88.toByte :: Nil, mnemonic, destination)

      protected def ALToMOffs8(memoryLocation: MemoryLocation with ByteSize): X86Operation =
        new Static(0xA2.toByte :: Nil, mnemonic) with MemoryLocationOperation[ByteSize] with NoImmediate {
          protected override def allOperands: Set[OperandInfo[_]] =
            super.allOperands + OperandInfo.implicitOperand(AL, source)

          override val location: OperandWithSizePrefixInfo[MemoryLocation with ByteSize] = memoryLocation

          override def offsetOrder: OperandOrder = destination

        }

      protected def R16ToRM16[Size <: MaxWideSize](operand1: GeneralPurposeRegister with Size, operand2: ModRMEncodableOperand with Size) =
        new ModRRM(operand1, operand2, 0x89.toByte :: Nil, mnemonic, destination)

      protected def AXToMOffs16[Size <: MaxWideSize](accumulatorRegister: AccumulatorRegister with Size, memoryLocation: MemoryLocation with Size): X86Operation =
        new Static(0xA3.toByte :: Nil, mnemonic) with MemoryLocationOperation[Size] with NoImmediate {
          protected override def allOperands: Set[OperandInfo[_]] =
            super.allOperands + OperandInfo.implicitOperand(accumulatorRegister, source)

          override val location: OperandWithSizePrefixInfo[MemoryLocation with Size] = memoryLocation

          override def offsetOrder: OperandOrder = destination
        }

      protected def RM8ToR8(operand1: ByteRegister, operand2: ModRMEncodableOperand with ByteSize) =
        new ModRRM(operand1, operand2, 0x8A.toByte :: Nil, mnemonic, source)

      protected def MOffs8ToAL(memoryLocation: MemoryLocation with ByteSize): X86Operation =
        new Static(0xA0.toByte :: Nil, mnemonic) with MemoryLocationOperation[ByteSize] with NoImmediate {
          protected override def allOperands: Set[OperandInfo[_]] =
            super.allOperands + OperandInfo.implicitOperand(AL, destination)

          override val location: OperandWithSizePrefixInfo[MemoryLocation with ByteSize] = memoryLocation

          override def offsetOrder: OperandOrder = source
        }

      protected def RM16ToR16[Size <: MaxWideSize](operand1: GeneralPurposeRegister with Size, operand2: ModRMEncodableOperand with Size) =
        new ModRRM(operand1, operand2, 0x8B.toByte :: Nil, mnemonic, source)

      protected def MOffs16ToAX[Size <: MaxWideSize](memoryLocation: MemoryLocation with Size, accumulatorRegister: AccumulatorRegister with Size): X86Operation =
        new Static(0xA1.toByte :: Nil, mnemonic) with MemoryLocationOperation[Size] with NoImmediate {
          protected override def allOperands: Set[OperandInfo[_]] =
            super.allOperands + OperandInfo.implicitOperand(accumulatorRegister, destination)

          override val location: OperandWithSizePrefixInfo[MemoryLocation with Size] = memoryLocation

          override def offsetOrder: OperandOrder = source
        }

      protected def Imm8ToR8(register: ByteRegister, immediateValue: ImmediateValue with ByteSize): X86Operation =
        new RegisterEncoded[ByteSize](register, Seq(0xB0.toByte), mnemonic) with NoDisplacement with Immediate[ByteSize] {
          override def immediate: OperandWithOperandSizePrefixInfo[ImmediateValue with ByteSize] = immediateValue
          override def immediateOrder: OperandOrder = source
          override def registerOrder: OperandOrder = destination
        }

      protected def Imm16ToR16[Size <: MaxWideSize](register: GeneralPurposeRegister with Size, immediateValue: ImmediateValue with Size): X86Operation =
        new RegisterEncoded[Size](register, Seq(0xB8.toByte), mnemonic) with NoDisplacement with Immediate[Size] {
          override def immediate: OperandWithOperandSizePrefixInfo[ImmediateValue with Size] = immediateValue
          override def immediateOrder: OperandOrder = source
          override def registerOrder: OperandOrder = destination
        }

      protected def Imm8ToRM8(operand: ModRMEncodableOperand with ByteSize, immediateValue: ImmediateValue with ByteSize): X86Operation =
        new ModRM(operand, 0xC6.toByte :: Nil, 0, mnemonic, destination) with NoDisplacement with Immediate[ByteSize] {
          override def immediate: OperandWithOperandSizePrefixInfo[ImmediateValue with ByteSize] = immediateValue
          override def immediateOrder: OperandOrder = source
        }

      protected def Imm16ToRM16[OperandSize <: MaxWideSize](operand: ModRMEncodableOperand with OperandSize, immediateValue: ImmediateValue with OperandSize): X86Operation =
        new ModRM(operand, 0xC7.toByte :: Nil, 0, mnemonic, destination) with NoDisplacement with Immediate[OperandSize] {
          override def immediate: OperandWithOperandSizePrefixInfo[ImmediateValue with OperandSize] = immediateValue
          override def immediateOrder: OperandOrder = source
        }

      sealed abstract class MoveForLabel(targetLabel: Label) extends AbsoluteReference(targetLabel) {
        def size: Int

        override def sizeForDistance(distance: Int): Int = size

        override def possibleSizes: Set[Int] = Set(size)
      }

      def apply(source: Accumulator.LowByte.type, destination: MemoryAddress with ByteSize): X86Operation =
        ALToMOffs8(destination)

      def apply(source: ByteRegister, destination: ModRMEncodableOperand with ByteSize): X86Operation =
        R8ToRM8(source, destination)

      def apply(source: MemoryAddress with ByteSize, accumulator: Accumulator.LowByte.type): X86Operation =
        MOffs8ToAL(source)

      def apply(source: ModRMEncodableOperand with ByteSize, destination: ByteRegister): X86Operation =
        RM8ToR8(destination, source)

      def apply(source: ByteRegister, destination: ByteRegister): X86Operation = {
        assume(!(source.isInstanceOf[GeneralPurposeRexRegister] && destination.isInstanceOf[HighByteRegister]))
        assume(!(source.isInstanceOf[HighByteRegister] && destination.isInstanceOf[GeneralPurposeRexRegister]))
        apply(source, destination.asInstanceOf[ModRMEncodableOperand with ByteSize])
      }

      def apply[Size <: MaxWideSize](source: ModRMEncodableOperand with Size, destination: SegmentRegister): X86Operation =
        RM16ToSReg(destination, source)

      def apply[Size <: MaxWideSize](source: SegmentRegister, destination: ModRMEncodableOperand with Size): X86Operation =
        SRegToRM16(source, destination)

      def apply[Size <: MaxWideSize](accumulator: AccumulatorRegister with Size, destination: MemoryAddress with Size): X86Operation =
        AXToMOffs16(accumulator, destination)

      def apply[Size <: MaxWideSize](source: GeneralPurposeRegister with Size, destination: ModRMEncodableOperand with Size): X86Operation =
        R16ToRM16(source, destination)

      def apply[Size <: MaxWideSize](source: GeneralPurposeRegister with Size, destination: GeneralPurposeRegister with Size): X86Operation =
        R16ToRM16(source, destination.asInstanceOf[ModRMEncodableOperand with Size])

      def apply[Size <: MaxWideSize](source: MemoryAddress with Size, accumulator: AccumulatorRegister with Size): X86Operation =
        MOffs16ToAX(source, accumulator)

      def apply[Size <: MaxWideSize](source: ModRMEncodableOperand with Size, destination: GeneralPurposeRegister with Size): X86Operation =
        RM16ToR16(destination, source)

      def apply[Size <: MaxWideSize](source: ImmediateValue with Size, destination: GeneralPurposeRegister with Size): X86Operation =
        Imm16ToR16(destination, source)

      def apply(source: ImmediateValue with ByteSize, destination: ByteRegister): X86Operation =
        Imm8ToR8(destination, source)

      def apply[Size <: MaxValueSize](source: ImmediateValue with Size, destination: ModRMEncodableOperand with Size): X86Operation =
        (source, destination) match {
          case (s: ImmediateValue with ByteSize, d: ModRMEncodableOperand with ByteSize) =>
            Imm8ToRM8(d, s)
          case (s: ImmediateValue with MaxWideSize @unchecked, d: ModRMEncodableOperand with MaxWideSize @unchecked) =>
            Imm16ToRM16(d, s)
        }
    }
  }

  sealed trait I8086 extends Common {
    self: ProcessorMode.LegacyBounds with OperandSizeInfo  =>
    sealed trait I8086Move extends BasicMove
  }

  sealed trait I386 extends Common {
    self: ProcessorMode.I386Bounds  with OperandSizeInfo =>
    sealed trait I386Move extends BasicMove
  }

  trait LegacyOperations extends I8086 {
    self: ProcessorMode.LegacyBounds with ImmediateValue.I8086Implicits  with OperandSizeInfo =>
    object Move extends I8086Move {
      def forLabel(targetLabel: Label, register: GeneralPurposeRegister with WordSize): AbsoluteReference =
        new MoveForLabel(targetLabel) {
          override val size: Int = 3

          override def encodableForDistance(distance: Int): UnlabeledEncodable =
            Imm16ToR16(register, wordImm(distance.toShort))
        }
    }
  }

  trait RealOperations extends I386 {
    self: ProcessorMode.I386Bounds with ImmediateValue.I8086Implicits with OperandSizeInfo  =>
    object Move extends I386Move {

      def forLabel(targetLabel: Label, register: GeneralPurposeRegister with WordSize): AbsoluteReference =
        new MoveForLabel(targetLabel) {
          override val size: Int = 3

          override def encodableForDistance(distance: Int): UnlabeledEncodable =
            Imm16ToR16(register, wordImm(distance.toShort))
        }
    }
  }

  trait ProtectedOperations extends I386 {
    self: ProcessorMode.I386Bounds with ImmediateValue.I386Implicits  with OperandSizeInfo =>
    object Move extends I386Move {

      def forLabel(targetLabel: Label, register: GeneralPurposeRegister with DoubleWordSize): AbsoluteReference =
        new MoveForLabel(targetLabel) {
          override val size: Int = 5

          override def encodableForDistance(distance: Int): UnlabeledEncodable =
            Imm16ToR16[DoubleWordSize](register, doubleWordImm(distance))
        }
    }
  }

  trait LongOperations extends Common {
    self: ProcessorMode.LongBounds with ImmediateValue.X64Implicits with OperandSizeInfo  =>
    object Move extends BasicMove {
      def forLabel(targetLabel: Label, register: GeneralPurposeRegister with QuadWordSize): AbsoluteReference =
        new MoveForLabel(targetLabel) {
          override val size: Int = 10

          override def encodableForDistance(distance: Int): UnlabeledEncodable =
            Imm16ToR16(register, quadWordImm(distance))
        }
    }
  }
}
