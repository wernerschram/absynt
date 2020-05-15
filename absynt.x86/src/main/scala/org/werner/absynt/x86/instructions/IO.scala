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

object IO extends {

  trait Common {
    self: ArchitectureBounds with OperandSizeInfo =>

    sealed trait I8086Input {
      val mnemonic: String = "in"

      private def Imm8ToAL(immediateValue: ImmediateValue[_] with ByteSize) =
        new Static(0xE4.toByte :: Nil, mnemonic) with NoDisplacement with Immediate[ByteSize] {
          protected override def allOperands: Set[OperandInfo[_]] =
            super.allOperands + OperandInfo.implicitOperand(Accumulator.LowByte, destination)
          override def immediateOrder: OperandOrder = source
          override val immediate: OperandWithOperandSizePrefixInfo[ImmediateValue[_] with ByteSize] = immediateValue
        }

      private def Imm8ToAX(immediateValue: ImmediateValue[_] with ByteSize) =
        new Static(0xE5.toByte :: Nil, mnemonic) with NoDisplacement with Immediate[ByteSize] {
          protected override def allOperands: Set[OperandInfo[_]] =
            super.allOperands + OperandInfo.implicitOperand(Accumulator.Word, destination)
          override def immediateOrder: OperandOrder = source
          override val immediate: OperandWithOperandSizePrefixInfo[ImmediateValue[_] with ByteSize] = immediateValue
        }

      private def DXToAL() = new Static(0xEC.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands +
            OperandInfo.implicitPort(Data.Word, source) +
            OperandInfo.implicitOperand(Accumulator.LowByte, destination)
      }

      private def DXToAX() = new Static(0xED.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands +
            OperandInfo.implicitPort(Data.Word, source) +
            OperandInfo.implicitOperand(Accumulator.Word, destination)
      }

      def apply(immediate: ImmediateValue[_] with ByteSize, destination: Accumulator.LowByte.type): Static =
        Imm8ToAL(immediate)

      def apply(immediate: ImmediateValue[_] with ByteSize, destination: Accumulator.Word.type): Static =
        Imm8ToAX(immediate)

      def apply(port: Data.Word.type, destination: Accumulator.LowByte.type): Static =
        DXToAL()

      def apply(port: Data.Word.type, destination: Accumulator.Word.type): Static =
        DXToAX()
    }

    sealed trait I8086Output {
      val mnemonic: String = "out"

      private def ALToImm8(immediateValue: ImmediateValue[_] with ByteSize) =
        new Static(0xE6.toByte :: Nil, mnemonic) with NoDisplacement with Immediate[ByteSize] {
          protected override def allOperands: Set[OperandInfo[_]] =
            super.allOperands + OperandInfo.implicitOperand(Accumulator.LowByte, source)
          override def immediateOrder: OperandOrder = destination
          override val immediate: OperandWithOperandSizePrefixInfo[ImmediateValue[_] with ByteSize] = immediateValue
        }

      private def AXToImm8(immediateValue: ImmediateValue[_] with ByteSize) =
        new Static(0xE7.toByte :: Nil, mnemonic) with NoDisplacement with Immediate[ByteSize] {
          protected override def allOperands: Set[OperandInfo[_]] =
            super.allOperands + OperandInfo.implicitOperand(Accumulator.Word, source)
          override def immediateOrder: OperandOrder = destination
          override val immediate: OperandWithOperandSizePrefixInfo[ImmediateValue[_] with ByteSize] = immediateValue
        }

      private def ALToDX() =
        new Static(0xEE.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
          protected override def allOperands: Set[OperandInfo[_]] =
            super.allOperands +
              OperandInfo.implicitPort(Data.Word, destination) +
              OperandInfo.implicitOperand(Accumulator.LowByte, source)
        }

      private def AXToDX() =
        new Static(0xEF.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate  {
          protected override def allOperands: Set[OperandInfo[_]] =
            super.allOperands +
              OperandInfo.implicitPort(Data.Word, destination) +
              OperandInfo.implicitOperand(Accumulator.Word, source)
        }

      def apply(destination: Accumulator.LowByte.type, immediate: ImmediateValue[_] with ByteSize): Static with Immediate[ByteSize] =
        ALToImm8(immediate)

      def apply(destination: Accumulator.Word.type, immediate: ImmediateValue[_] with ByteSize): Static with Immediate[ByteSize] =
        AXToImm8(immediate)


      def apply(destination: Accumulator.LowByte.type, port: Data.Word.type): Static =
        ALToDX()

      def apply(destination: Accumulator.Word.type, port: Data.Word.type): Static =
        AXToDX()
    }
  }



  trait LegacyOperations extends Common {
    self: ProcessorMode.LegacyBounds with OperandSizeInfo =>

    object Input extends I8086Input
    object Output extends I8086Output
  }

  trait I386Operations extends Common {
    self: ArchitectureBounds with OperandSizeInfo =>

    sealed trait I386Input extends I8086Input {
      private def Imm8ToEAX(immediateValue: ImmediateValue[_] with ByteSize) =
        new Static(0xE5.toByte :: Nil, mnemonic) with NoDisplacement with Immediate[ByteSize] {
          protected override def allOperands: Set[OperandInfo[_]] =
            super.allOperands + OperandInfo.implicitOperand(Accumulator.DoubleWord, destination)
          override def immediateOrder: OperandOrder = source
          override val immediate: OperandWithOperandSizePrefixInfo[ImmediateValue[_] with ByteSize] = immediateValue
        }

      private def DXToEAX() = new Static(0xED.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands +
            OperandInfo.implicitPort(Data.Word, source) +
            OperandInfo.implicitOperand(Accumulator.DoubleWord, destination)
      }

      def apply(immediate: ImmediateValue[_] with ByteSize, destination: Accumulator.DoubleWord.type): Static =
        Imm8ToEAX(immediate)

      def apply(port: Data.Word.type, destination: Accumulator.DoubleWord.type): Static =
        DXToEAX()
    }


    sealed trait I386Output extends I8086Output {
      private def EAXToImm8(immediateValue: ImmediateValue[_] with ByteSize) =
        new Static(0xE7.toByte :: Nil, mnemonic) with NoDisplacement with Immediate[ByteSize] {
          protected override def allOperands: Set[OperandInfo[_]] =
            super.allOperands + OperandInfo.implicitOperand(Accumulator.DoubleWord, source)
          override def immediateOrder: OperandOrder = destination
          override val immediate: OperandWithOperandSizePrefixInfo[ImmediateValue[_] with ByteSize] = immediateValue
        }


      private def EAXToDX() =
        new Static(0xEF.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate  {
          protected override def allOperands: Set[OperandInfo[_]] =
            super.allOperands +
              OperandInfo.implicitPort(Data.Word, destination) +
              OperandInfo.implicitOperand(Accumulator.DoubleWord, source)
        }

      def apply(destination: Accumulator.DoubleWord.type, port: Data.Word.type): Static =
        EAXToDX()

      def apply(destination: Accumulator.DoubleWord.type, immediate: ImmediateValue[_] with ByteSize): Static with Immediate[ByteSize] =
        EAXToImm8(immediate)
    }

    object Input extends I386Input
    object Output extends I386Output
  }

  trait LongOperations extends I386Operations {
    self: ProcessorMode.LongBounds with OperandSizeInfo =>
  }

}
