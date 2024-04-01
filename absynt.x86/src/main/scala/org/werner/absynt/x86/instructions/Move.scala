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
    self: ArchitectureBounds & ProcessorMode & OperandSizeInfo  =>

    trait BasicMove {

      protected def RM16ToSReg[Size <: MaxWideSize](operand1: SegmentRegister, operand2: ModRMEncodableOperand & Size) =
        new ModSegmentRM(operand1, operand2, 0x8E.toByte :: Nil, mnemonic, source) with NoDisplacement with NoImmediate

      protected def SRegToRM16[Size <: MaxWideSize](operand1: SegmentRegister, operand2: ModRMEncodableOperand & Size) =
        new ModSegmentRM(operand1, operand2, 0x8C.toByte :: Nil, mnemonic, destination) with NoDisplacement with NoImmediate

      protected def R8ToRM8(operand1: ByteRegister, operand2: ModRMEncodableOperand & ByteSize) =
        new ModRRM(operand1, operand2, 0x88.toByte :: Nil, mnemonic, destination)

      protected def ALToMOffs8(memoryLocation: MemoryLocation & ByteSize): X86Operation =
        new Static(0xA2.toByte :: Nil, mnemonic) with MemoryLocationOperation[ByteSize] with NoImmediate {
          protected override def allOperands: Set[OperandInfo[?]] =
            super.allOperands + OperandInfo.implicitOperand(AL, source)

          override val location: OperandWithSizePrefixInfo[MemoryLocation & ByteSize] = memoryLocation

          override def offsetOrder: OperandOrder = destination

        }

      protected def R16ToRM16[Size <: MaxWideSize](operand1: GeneralPurposeRegister & Size, operand2: ModRMEncodableOperand & Size) =
        new ModRRM(operand1, operand2, 0x89.toByte :: Nil, mnemonic, destination)

      protected def AXToMOffs16[Size <: MaxWideSize](accumulatorRegister: AccumulatorRegister & Size, memoryLocation: MemoryLocation & Size): X86Operation =
        new Static(0xA3.toByte :: Nil, mnemonic) with MemoryLocationOperation[Size] with NoImmediate {
          protected override def allOperands: Set[OperandInfo[?]] =
            super.allOperands + OperandInfo.implicitOperand(accumulatorRegister, source)

          override val location: OperandWithSizePrefixInfo[MemoryLocation & Size] = memoryLocation

          override def offsetOrder: OperandOrder = destination
        }

      protected def RM8ToR8(operand1: ByteRegister, operand2: ModRMEncodableOperand & ByteSize) =
        new ModRRM(operand1, operand2, 0x8A.toByte :: Nil, mnemonic, source)

      protected def MOffs8ToAL(memoryLocation: MemoryLocation & ByteSize): X86Operation =
        new Static(0xA0.toByte :: Nil, mnemonic) with MemoryLocationOperation[ByteSize] with NoImmediate {
          protected override def allOperands: Set[OperandInfo[?]] =
            super.allOperands + OperandInfo.implicitOperand(AL, destination)

          override val location: OperandWithSizePrefixInfo[MemoryLocation & ByteSize] = memoryLocation

          override def offsetOrder: OperandOrder = source
        }

      protected def RM16ToR16[Size <: MaxWideSize](operand1: GeneralPurposeRegister & Size, operand2: ModRMEncodableOperand & Size) =
        new ModRRM(operand1, operand2, 0x8B.toByte :: Nil, mnemonic, source)

      protected def MOffs16ToAX[Size <: MaxWideSize](memoryLocation: MemoryLocation & Size, accumulatorRegister: AccumulatorRegister & Size): X86Operation =
        new Static(0xA1.toByte :: Nil, mnemonic) with MemoryLocationOperation[Size] with NoImmediate {
          protected override def allOperands: Set[OperandInfo[?]] =
            super.allOperands + OperandInfo.implicitOperand(accumulatorRegister, destination)

          override val location: OperandWithSizePrefixInfo[MemoryLocation & Size] = memoryLocation

          override def offsetOrder: OperandOrder = source
        }

      protected def Imm8ToR8(register: ByteRegister, immediateValue: ImmediateValue[?] & ByteSize): X86Operation =
        new RegisterEncoded[ByteSize](register, Seq(0xB0.toByte), destination, mnemonic) with NoDisplacement with Immediate[ByteSize](immediateValue, source)

      protected def Imm16ToR16[Size <: MaxWideSize](register: GeneralPurposeRegister & Size, immediateValue: ImmediateValue[?] & Size): X86Operation =
        new RegisterEncoded[Size](register, Seq(0xB8.toByte), destination, mnemonic) with NoDisplacement with Immediate[Size](immediateValue, source)

      protected def Imm8ToRM8(operand: ModRMEncodableOperand & ByteSize, immediateValue: ImmediateValue[?] & ByteSize) =
        new ModRM(operand, 0xC6.toByte :: Nil, 0, mnemonic, destination) with NoDisplacement with Immediate[ByteSize](immediateValue, source)

      protected def Imm16ToRM16[OperandSize <: MaxWideSize](operand: ModRMEncodableOperand & OperandSize, immediateValue: ImmediateValue[?] & OperandSize) =
        new ModRM(operand, 0xC7.toByte :: Nil, 0, mnemonic, destination) with NoDisplacement with Immediate[OperandSize](immediateValue, source)

      sealed abstract class MoveForLabel(targetLabel: Label) extends AbsoluteReference(targetLabel) {
        def size: Int

        override def sizeForDistance(distance: Int): Int = size

        override def possibleSizes: Set[Int] = Set(size)
      }

      def apply(source: Accumulator.LowByte.type, destination: MemoryAddress & ByteSize): X86Operation =
        ALToMOffs8(destination)

      def apply(source: ByteRegister, destination: ModRMEncodableOperand & ByteSize): X86Operation =
        R8ToRM8(source, destination)

      def apply(source: MemoryAddress & ByteSize, accumulator: Accumulator.LowByte.type): X86Operation =
        MOffs8ToAL(source)

      def apply(source: ModRMEncodableOperand & ByteSize, destination: ByteRegister): X86Operation =
        RM8ToR8(destination, source)

      def apply(source: ByteRegister, destination: ByteRegister): X86Operation = {
        assume(!(source.isInstanceOf[GeneralPurposeRexRegister] && destination.isInstanceOf[HighByteRegister]))
        assume(!(source.isInstanceOf[HighByteRegister] && destination.isInstanceOf[GeneralPurposeRexRegister]))
        apply(source, destination.asInstanceOf[ModRMEncodableOperand & ByteSize])
      }

      def apply[Size <: MaxWideSize](source: ModRMEncodableOperand & Size, destination: SegmentRegister) =
        RM16ToSReg(destination, source)

      def apply[Size <: MaxWideSize](source: SegmentRegister, destination: ModRMEncodableOperand & Size) =
        SRegToRM16(source, destination)

      def apply[Size <: MaxWideSize](accumulator: AccumulatorRegister & Size, destination: MemoryAddress & Size): X86Operation =
        AXToMOffs16(accumulator, destination)

      def apply[Size <: MaxWideSize](source: GeneralPurposeRegister & Size, destination: ModRMEncodableOperand & Size) =
        R16ToRM16(source, destination)

      def apply[Size <: MaxWideSize](source: GeneralPurposeRegister & Size, destination: GeneralPurposeRegister & Size): X86Operation =
        R16ToRM16(source, destination.asInstanceOf[ModRMEncodableOperand & Size])

      def apply[Size <: MaxWideSize](source: MemoryAddress & Size, accumulator: AccumulatorRegister & Size): X86Operation =
        MOffs16ToAX(source, accumulator)

      def apply[Size <: MaxWideSize](source: ModRMEncodableOperand & Size, destination: GeneralPurposeRegister & Size): X86Operation =
        RM16ToR16(destination, source)

      def apply[Size <: MaxWideSize](source: ImmediateValue[?] & Size, destination: GeneralPurposeRegister & Size): X86Operation =
        Imm16ToR16(destination, source)

      def apply(source: ImmediateValue[?] & ByteSize, destination: ByteRegister): X86Operation =
        Imm8ToR8(destination, source)

      def apply[Size <: MaxValueSize](source: ImmediateValue[?] & Size, destination: ModRMEncodableOperand & Size) = {
        source match {
          case s: ByteSize =>
            Imm8ToRM8(destination.asInstanceOf[ModRMEncodableOperand & ByteSize], source.asInstanceOf[ImmediateValue[?] & ByteSize])
          case s: WordDoubleQuadSize =>
            Imm16ToRM16(destination.asInstanceOf[ModRMEncodableOperand & MaxWideSize], source.asInstanceOf[ImmediateValue[?] & MaxWideSize])
        }
      }
    }
  }

  sealed trait I8086 extends Common {
    self: ProcessorMode.LegacyBounds & ProcessorMode & OperandSizeInfo  =>
    sealed trait I8086Move extends BasicMove
  }

  sealed trait I386 extends Common {
    self: ProcessorMode.I386Bounds & ProcessorMode & OperandSizeInfo =>
    sealed trait I386Move extends BasicMove
  }

  trait LegacyOperations extends I8086 {
    self: ProcessorMode.LegacyBounds & ProcessorMode & ImmediateValue.I8086Implicits & OperandSizeInfo =>
    object Move extends I8086Move {
      def forLabel(targetLabel: Label, register: GeneralPurposeRegister & WordSize): AbsoluteReference =
        new MoveForLabel(targetLabel) {
          override val size: Int = 3

          override def encodableForDistance(distance: Int): UnlabeledEncodable =
            Imm16ToR16(register, wordImm(distance.toShort))
        }
    }
  }

  trait RealOperations extends I386 {
    self: ProcessorMode.I386Bounds & ProcessorMode & ImmediateValue.I8086Implicits & OperandSizeInfo  =>
    object Move extends I386Move {

      def forLabel(targetLabel: Label, register: GeneralPurposeRegister & WordSize): AbsoluteReference =
        new MoveForLabel(targetLabel) {
          override val size: Int = 3

          override def encodableForDistance(distance: Int): UnlabeledEncodable =
            Imm16ToR16(register, wordImm(distance.toShort))
        }
    }
  }

  trait ProtectedOperations extends I386 {
    self: ProcessorMode.I386Bounds & ProcessorMode & ImmediateValue.I386Implicits & OperandSizeInfo  =>
    object Move extends I386Move {

      def forLabel(targetLabel: Label, register: GeneralPurposeRegister & DoubleWordSize): AbsoluteReference =
        new MoveForLabel(targetLabel) {
          override val size: Int = 5

          override def encodableForDistance(distance: Int): UnlabeledEncodable =
            Imm16ToR16[DoubleWordSize](register, doubleWordImm(distance))
        }
    }
  }

  trait LongOperations extends Common {
    self: ProcessorMode.LongBounds & ProcessorMode & ImmediateValue.X64Implicits & OperandSizeInfo  =>
    object Move extends BasicMove {
      def forLabel(targetLabel: Label, register: GeneralPurposeRegister & QuadWordSize): AbsoluteReference =
        new MoveForLabel(targetLabel) {
          override val size: Int = 10

          override def encodableForDistance(distance: Int): UnlabeledEncodable =
            Imm16ToR16(register, quadWordImm(distance))
        }
    }
  }
}
