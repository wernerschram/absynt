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

object BasicInteraction {

  sealed trait Common {
    self: ArchitectureBounds & ProcessorMode & OperandSizeInfo =>
    protected def Imm8ToAL(immediateValue: ImmediateValue[?] & ByteSize, opcodeBase: Byte, mnemonic: String): X86Operation =
      new Static((opcodeBase + 0x04).toByte :: Nil, mnemonic) 
        with NoDisplacement 
        with Immediate[ByteSize](immediateValue, source) 
        with ExtraOperands(OperandInfo.implicitOperand(Accumulator.LowByte, destination))

    protected def Imm16ToAX(immediateValue: ImmediateValue[?] & WordSize, opcodeBase: Byte, mnemonic: String): X86Operation =
      new Static((opcodeBase + 0x05).toByte :: Nil, mnemonic) 
        with NoDisplacement 
        with Immediate[WordSize](immediateValue, source) 
        with ExtraOperands(OperandInfo.implicitOperand(Accumulator.Word, destination))

    protected def Imm32ToEAX(immediateValue: ImmediateValue[?] & DoubleWordSize, opcodeBase: Byte, mnemonic: String): X86Operation =
      new Static((opcodeBase + 0x5).toByte :: Nil, mnemonic) 
        with NoDisplacement 
        with Immediate[DoubleWordSize](immediateValue, source) 
        with ExtraOperands(OperandInfo.implicitOperand(Accumulator.DoubleWord, destination))

    protected def Imm32ToRAX(immediateValue: ImmediateValue[?] & DoubleWordSize, opcodeBase: Byte, mnemonic: String): X86Operation =
      new Static((opcodeBase + 0x5).toByte :: Nil, mnemonic) 
        with NoDisplacement 
        with Immediate[DoubleWordSize](immediateValue, source) 
        with ExtraOperands(OperandInfo.implicitOperand(Accumulator.QuadWord, destination))

    protected def Imm8ToRM8(operand: ModRMEncodableOperand & ByteSize, immediateValue: ImmediateValue[?] & ByteSize, extensionCode: Byte, mnemonic: String): X86Operation =
      new ModRM(operand, 0x80.toByte :: Nil, extensionCode, mnemonic, destination) 
        with NoDisplacement 
        with Immediate[ByteSize](immediateValue, source)
      

    protected def Imm16ToRM16[Size <: MaxWideSize](operand: ModRMEncodableOperand & Size, immediateValue: ImmediateValue[?] & Size, extensionCode: Byte, mnemonic: String): X86Operation =
      new ModRM(operand, 0x81.toByte :: Nil, extensionCode, mnemonic, destination)
        with NoDisplacement 
        with Immediate[Size](immediateValue, source)

    protected def Imm8ToRM16[Size <: MaxWideSize](operand: ModRMEncodableOperand & Size, immediateValue: ImmediateValue[?] & ByteSize, extensionCode: Byte, mnemonic: String): X86Operation =
      new ModRM(operand, 0x83.toByte :: Nil, extensionCode, mnemonic, destination) 
        with NoDisplacement 
        with Immediate[ByteSize](immediateValue, source)

    protected def R8ToRM8(operand1: ByteRegister, operand2: ModRMEncodableOperand & ByteSize, opcodeBase: Byte, mnemonic: String): X86Operation =
      new ModRRM(operand1, operand2, (opcodeBase + 0x00).toByte :: Nil, mnemonic, destination)

    protected def R16ToRM16[Size <: MaxWideSize](operand1: GeneralPurposeRegister & Size, operand2: ModRMEncodableOperand & Size, opcodeBase: Byte, mnemonic: String): X86Operation =
      new ModRRM(operand1, operand2, (opcodeBase + 0x01).toByte :: Nil, mnemonic, destination)

    protected def RM8ToR8(operand1: ByteRegister, operand2: ModRMEncodableOperand & ByteSize, opcodeBase: Byte, mnemonic: String): X86Operation =
      new ModRRM(operand1, operand2, (opcodeBase + 0x02).toByte :: Nil, mnemonic, source)

    protected def RM16ToR16[Size <: MaxWideSize](operand1: GeneralPurposeRegister & Size, operand2: ModRMEncodableOperand & Size, opcodeBase: Byte, mnemonic: String) =
      new ModRRM(operand1, operand2, (opcodeBase + 0x03).toByte :: Nil, mnemonic, source)

    protected def RM8(operand: ModRMEncodableOperand & ByteSize, mnemonic: String): X86Operation =
      new ModRM(operand, 0xF6.toByte :: Nil, 2, mnemonic, destination) 
        with NoDisplacement 
        with NoImmediate

    protected def RM16[Size <: MaxWideSize](operand: ModRMEncodableOperand & Size, mnemonic: String): X86Operation =
      new ModRM(operand, 0xF7.toByte :: Nil, 2, mnemonic, destination) 
        with NoDisplacement 
        with NoImmediate

    sealed abstract class BasicInteraction[MaxValSize](opcodeBase: Byte, mnemonic: String) {

      def apply(source: ByteRegister, destination: ModRMEncodableOperand & ByteSize): X86Operation =
        R8ToRM8(source, destination, opcodeBase, mnemonic)

      def apply(source: ByteRegister, destination: ByteRegister): X86Operation =
        R8ToRM8(source, destination, opcodeBase, mnemonic)

      def apply[Size <: MaxWideSize](source: GeneralPurposeRegister & Size, destination: ModRMEncodableOperand & Size): X86Operation =
        R16ToRM16(source, destination, opcodeBase, mnemonic)

      def apply[Size <: MaxWideSize](source: GeneralPurposeRegister & Size, destination: GeneralPurposeRegister & Size): X86Operation =
        R16ToRM16(source, destination, opcodeBase, mnemonic)

      def apply(source: ModRMEncodableOperand & ByteSize, destination: ByteRegister): X86Operation =
        RM8ToR8(destination, source, opcodeBase, mnemonic)

      def apply[Size <: MaxWideSize](source: ModRMEncodableOperand & Size, destination: GeneralPurposeRegister & Size): X86Operation =
        RM16ToR16(destination, source, opcodeBase, mnemonic)
    }

    object Not {
      val mnemonic: String = "not"

      def apply(operand: ModRMEncodableOperand & MaxValueSize): X86Operation =
        operand match {
          case o: ByteSize => RM8(o, mnemonic)
          case o: MaxWideSize @unchecked => RM16(o, mnemonic)
        }
    }
  }


  trait LegacyOperations extends Common {
    self: ProcessorMode.LegacyBounds & ProcessorMode & OperandSizeInfo =>

    sealed class I8086BasicInteraction(opcodeBase: Byte, extensionCode: Byte, mnemonic: String) extends BasicInteraction[MaxValueSize](opcodeBase, mnemonic) {
      def apply(immediate: ImmediateValue[?] & ByteSize, destination: Accumulator.LowByte.type): X86Operation =
        Imm8ToAL(immediate, opcodeBase, mnemonic)

      def apply(immediate: ImmediateValue[?] & WordSize, destination: Accumulator.Word.type): X86Operation =
        Imm16ToAX(immediate, opcodeBase, mnemonic)

      def apply[ImmediateSize <: MaxValueSize, DestinationSize <: MaxValueSize](immediate: ImmediateValue[?] & ImmediateSize, destination: ModRMEncodableOperand & DestinationSize): X86Operation =
        (immediate, destination) match {
          case (imm: ByteSize, d: WordSize) =>
            Imm8ToRM16(d, imm, extensionCode, mnemonic)
          case (imm: ByteSize, d: ByteSize) =>
            Imm8ToRM8(d, imm, extensionCode, mnemonic)
          case (imm: WordSize, d: WordSize) =>
            Imm16ToRM16(d, imm, extensionCode, mnemonic)
          case _ =>
            throw new AssertionError
        }
    }

    object Add extends I8086BasicInteraction(0x00.toByte, 0x00.toByte, "add")
    object AddCarry extends I8086BasicInteraction(0x10.toByte, 0x02.toByte, "adc")
    object And extends I8086BasicInteraction(0x20.toByte, 0x04.toByte, "and")
    object Compare extends I8086BasicInteraction(0x38.toByte, 0x07.toByte, "cmp")
    object Or extends I8086BasicInteraction(0x08.toByte, 0x01.toByte, "or")
    object Subtract extends I8086BasicInteraction(0x28.toByte, 0x05.toByte, "sub")
    object SubtractCarry extends I8086BasicInteraction(0x18.toByte, 0x03.toByte, "sbc")
    object Xor extends I8086BasicInteraction(0x30.toByte, 0x06.toByte, "xor")
  }

  trait I386Operations extends Common {
    self: ProcessorMode.I386Bounds & ProcessorMode & OperandSizeInfo =>

    sealed class I386BasicInteraction(opcodeBase: Byte, extensionCode: Byte, mnemonic: String) extends BasicInteraction[MaxValueSize](opcodeBase, mnemonic) {
      def apply(immediate: ImmediateValue[?] & ByteSize, destination: Accumulator.LowByte.type): X86Operation =
        Imm8ToAL(immediate, opcodeBase, mnemonic)

      def apply(immediate: ImmediateValue[?] & WordSize, destination: Accumulator.Word.type): X86Operation =
        Imm16ToAX(immediate, opcodeBase, mnemonic)

      def apply(immediate: ImmediateValue[?] & DoubleWordSize, destination: Accumulator.DoubleWord.type): X86Operation =
        Imm32ToEAX(immediate, opcodeBase, mnemonic)

      def apply[ImmediateSize <: MaxValueSize, DestinationSize <: MaxValueSize](immediate: ImmediateValue[?] & ImmediateSize, destination: ModRMEncodableOperand & DestinationSize): X86Operation =
        (immediate, destination) match {
          case (imm: ByteSize, d: (WordSize | DoubleWordSize)) =>
            Imm8ToRM16(d, imm, extensionCode, mnemonic)
          case (imm: ByteSize, d: ByteSize) =>
            Imm8ToRM8(d, imm, extensionCode, mnemonic)
          case (imm: (WordSize | DoubleWordSize), d: (WordSize | DoubleWordSize))
            if d `sizeEquals` imm =>
            Imm16ToRM16(d, imm, extensionCode, mnemonic)
          case _ =>
            throw new AssertionError
        }
    }

    object Add extends I386BasicInteraction(0x00.toByte, 0x00.toByte, "add")
    object AddCarry extends I386BasicInteraction(0x10.toByte, 0x02.toByte, "adc")
    object And extends I386BasicInteraction(0x20.toByte, 0x04.toByte, "and")
    object Compare extends I386BasicInteraction(0x38.toByte, 0x07.toByte, "cmp")
    object Or extends I386BasicInteraction(0x08.toByte, 0x01.toByte, "or")
    object Subtract extends I386BasicInteraction(0x28.toByte, 0x05.toByte, "sub")
    object SubtractBorrow extends I386BasicInteraction(0x18.toByte, 0x03.toByte, "sbb")
    object Xor extends I386BasicInteraction(0x30.toByte, 0x06.toByte, "xor")
  }

  trait LongOperations extends Common {
    self: ProcessorMode.LongBounds & ProcessorMode & OperandSizeInfo =>

    sealed class X64BasicInteraction(opcodeBase: Byte, extensionCode: Byte, mnemonic: String) extends BasicInteraction[MaxValueSize](opcodeBase, mnemonic) {
      def apply(immediate: ImmediateValue[?] & DoubleWordSize, destination: Accumulator.DoubleWord.type): X86Operation =
        Imm32ToEAX(immediate, opcodeBase, mnemonic)

      def apply(immediate: ImmediateValue[?] & DoubleWordSize, destination: Accumulator.QuadWord.type): X86Operation =
        Imm32ToRAX(immediate, opcodeBase, mnemonic)

      def apply[ImmediateSize <: ByteSize | WordSize | DoubleWordSize, DestinationSize <: MaxValueSize](immediate: ImmediateValue[?] & ImmediateSize, destination: ModRMEncodableOperand & DestinationSize): X86Operation =
        (immediate, destination) match {
          case (imm: ByteSize, d: (WordSize | DoubleWordSize | QuadWordSize)) =>
            Imm8ToRM16(d, imm, extensionCode, mnemonic)
          case (imm: ByteSize, d: ByteSize) =>
            Imm8ToRM8(d, imm, extensionCode, mnemonic)
          case (imm: DoubleWordSize, d: QuadWordSize) =>
            Imm16ToRM16(d, imm, extensionCode, mnemonic)
          case (imm: (WordSize | DoubleWordSize), d: (WordSize | DoubleWordSize))
            if d `sizeEquals` imm =>
            Imm16ToRM16(d, imm, extensionCode, mnemonic)
          case _ =>
            throw new AssertionError
        }
    }

    object Add extends X64BasicInteraction(0x00.toByte, 0x00.toByte, "add")
    object AddCarry extends X64BasicInteraction(0x10.toByte, 0x02.toByte, "adc")
    object And extends X64BasicInteraction(0x20.toByte, 0x04.toByte, "and")
    object Compare extends X64BasicInteraction(0x38.toByte, 0x07.toByte, "cmp")
    object Or extends X64BasicInteraction(0x08.toByte, 0x01.toByte, "or")
    object Subtract extends X64BasicInteraction(0x28.toByte, 0x05.toByte, "sub")
    object SubtractCarry extends X64BasicInteraction(0x18.toByte, 0x03.toByte, "sbc")
    object Xor extends X64BasicInteraction(0x30.toByte, 0x06.toByte, "xor")
  }

}
