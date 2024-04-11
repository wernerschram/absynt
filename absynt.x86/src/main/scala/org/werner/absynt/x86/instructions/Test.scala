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
import scala.language.implicitConversions

object Test {

  sealed trait Common {
    self: ArchitectureBounds & ProcessorMode & OperandSizeInfo =>
    val mnemonic = "test"

    protected def Imm8ToAL(immediateValue: ImmediateValue[?] & ByteSize): X86Operation =
      new Static(0xA8.toByte :: Nil, mnemonic)
        with NoDisplacement
        with Immediate[ByteSize](immediateValue, source)
        with ExtraOperands(OperandInfo.implicitOperand(Accumulator.LowByte, destination))

    protected def Imm16ToAX(immediateValue: ImmediateValue[?] & WordSize): X86Operation =
      new Static(0xA9.toByte :: Nil, mnemonic)
        with NoDisplacement
        with Immediate[WordSize](immediateValue, source)
        with ExtraOperands(OperandInfo.implicitOperand(Accumulator.Word, destination))

    protected def Imm32ToEAX(immediateValue: ImmediateValue[?] & DoubleWordSize): X86Operation =
      new Static(0xA9.toByte :: Nil, mnemonic)
        with NoDisplacement
        with Immediate[DoubleWordSize](immediateValue, source)
        with ExtraOperands(OperandInfo.implicitOperand(Accumulator.DoubleWord, destination))

    protected def Imm32ToRAX(immediateValue: ImmediateValue[?] & DoubleWordSize): X86Operation =
      new Static(0xA9.toByte :: Nil, mnemonic)
        with NoDisplacement
        with Immediate[DoubleWordSize](immediateValue, source)
        with ExtraOperands(OperandInfo.implicitOperand(Accumulator.QuadWord, destination))

    protected def Imm8ToRM8(operand: ModRMEncodableOperand & ByteSize, immediateValue: ImmediateValue[?] & ByteSize) =
      new ModRM(operand, 0xF6.toByte :: Nil, 0x00.toByte, mnemonic, destination)
        with NoDisplacement
        with Immediate[ByteSize](immediateValue, source)


    protected def Imm16ToRM16[Size <: WordSize | DoubleWordSize | QuadWordSize](operand: ModRMEncodableOperand & Size, immediateValue: ImmediateValue[?] & Size) =
      new ModRM(operand, 0xF7.toByte :: Nil, 0x00.toByte, mnemonic, destination)
        with NoDisplacement
        with Immediate[Size](immediateValue, source)

    protected def R8ToRM8(operand1: ByteRegister, operand2: ModRMEncodableOperand & ByteSize): X86Operation =
      new ModRRM(operand1, operand2, 0x84.toByte :: Nil, mnemonic, destination)

    protected def R16ToRM16[Size <: WordSize | DoubleWordSize | QuadWordSize](operand1: GeneralPurposeRegister & Size, operand2: ModRMEncodableOperand & Size): X86Operation =
      new ModRRM(operand1, operand2, 0x85.toByte :: Nil, mnemonic, destination)

    sealed abstract class TestBase {
      def apply(source: ByteRegister, destination: ModRMEncodableOperand & ByteSize): X86Operation =
        R8ToRM8(source, destination)

      def apply(source: ByteRegister, destination: ByteRegister): X86Operation =
        R8ToRM8(source, destination)

      def apply[Size <: MaxWideSize](source: GeneralPurposeRegister & Size, destination: ModRMEncodableOperand & Size): X86Operation =
        R16ToRM16(source, destination)

      def apply[Size <: MaxWideSize](source: GeneralPurposeRegister & Size, destination: GeneralPurposeRegister & Size): X86Operation =
        R16ToRM16(source, destination)
    }
  }


  trait LegacyOperations extends Common {
    self: ProcessorMode.LegacyBounds & ProcessorMode & OperandSizeInfo =>

    object Test extends TestBase  {

      def apply[Size <: ByteSize | WordSize](immediate: ImmediateValue[?] & Size, destination: ModRMEncodableOperand & Size): X86Operation = {
        immediate match {
          case i: ByteSize =>
            Imm8ToRM8(destination.asInstanceOf[ModRMEncodableOperand & ByteSize], immediate.asInstanceOf[ImmediateValue[?] & ByteSize])
          case i: WordSize =>
            Imm16ToRM16(destination.asInstanceOf[ModRMEncodableOperand & WordSize], immediate.asInstanceOf[ImmediateValue[?] & WordSize])
        }
      }
    }
  }

  trait I386Operations extends Common {
    self: ProcessorMode.I386Bounds & ProcessorMode & OperandSizeInfo =>

    object Test extends TestBase  {
      def apply(immediate: ImmediateValue[?] & ByteSize, destination: Accumulator.LowByte.type): X86Operation =
        Imm8ToAL(immediate)

      def apply(immediate: ImmediateValue[?] & WordSize, destination: Accumulator.Word.type): X86Operation =
        Imm16ToAX(immediate)

      def apply(immediate: ImmediateValue[?] & DoubleWordSize, destination: Accumulator.DoubleWord.type): X86Operation =
        Imm32ToEAX(immediate)

      def apply[Size <: ByteSize | WordSize | DoubleWordSize](immediate: ImmediateValue[?] & Size, destination: ModRMEncodableOperand & Size): X86Operation =
        immediate match {
          case i: ByteSize =>
            Imm8ToRM8(destination.asInstanceOf[ModRMEncodableOperand & ByteSize], immediate.asInstanceOf[ImmediateValue[?] & ByteSize])
          case i: (WordSize | DoubleWordSize) =>
            Imm16ToRM16(destination.asInstanceOf[ModRMEncodableOperand & (WordSize | DoubleWordSize)], immediate.asInstanceOf[ImmediateValue[?] & (WordSize | DoubleWordSize)])
        }
    }
  }

  trait LongOperations extends Common {
    self: ProcessorMode.LongBounds & ProcessorMode & OperandSizeInfo =>

    object Test extends TestBase {
      def apply(immediate: ImmediateValue[?] & ByteSize, destination: Accumulator.LowByte.type): X86Operation =
        Imm8ToAL(immediate)

      def apply(immediate: ImmediateValue[?] & WordSize, destination: Accumulator.Word.type): X86Operation =
        Imm16ToAX(immediate)

      def apply(immediate: ImmediateValue[?] & DoubleWordSize, destination: Accumulator.DoubleWord.type): X86Operation =
        Imm32ToEAX(immediate)

      def apply(immediate: ImmediateValue[?] & DoubleWordSize, destination: Accumulator.QuadWord.type): X86Operation =
        Imm32ToRAX(immediate)

      def apply[ImmediateSize <: ByteSize | WordSize | DoubleWordSize, DestinationSize <: ValueSize](immediate: ImmediateValue[?] & ImmediateSize, destination: ModRMEncodableOperand & DestinationSize): X86Operation =
        (immediate, destination) match {
          case (imm: ByteSize, d: ByteSize) =>
            Imm8ToRM8(d, imm)
          case (imm: DoubleWordSize, d: QuadWordSize) =>
            Imm16ToRM16(d, imm)
          case (imm: (WordSize | DoubleWordSize), d: (WordSize | DoubleWordSize))
            if d `sizeEquals` imm =>
            Imm16ToRM16(d, imm)
          case _ =>
            throw new AssertionError
        }
    }
  }
}
