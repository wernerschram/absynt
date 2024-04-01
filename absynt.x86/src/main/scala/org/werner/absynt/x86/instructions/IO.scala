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

object IO {

  trait Common {
    self: ArchitectureBounds & ProcessorMode & OperandSizeInfo =>

    sealed trait I8086Input {
      val mnemonic: String = "in"

      private def Imm8ToAL(immediateValue: ImmediateValue[?] & ByteSize) =
        new Static(0xE4.toByte :: Nil, mnemonic) with NoDisplacement with Immediate[ByteSize](immediateValue, source) {
          protected override def allOperands: Set[OperandInfo[?]] =
            super.allOperands + OperandInfo.implicitOperand(Accumulator.LowByte, destination)
        }

      private def Imm8ToAX(immediateValue: ImmediateValue[?] & ByteSize) =
        new Static(0xE5.toByte :: Nil, mnemonic) with NoDisplacement with Immediate[ByteSize](immediateValue, source) {
          protected override def allOperands: Set[OperandInfo[?]] =
            super.allOperands + OperandInfo.implicitOperand(Accumulator.Word, destination)
        }

      private def DXToAL() = new Static(0xEC.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[?]] =
          super.allOperands +
            OperandInfo.implicitPort(Data.Word, source) +
            OperandInfo.implicitOperand(Accumulator.LowByte, destination)
      }

      private def DXToAX() = new Static(0xED.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[?]] =
          super.allOperands +
            OperandInfo.implicitPort(Data.Word, source) +
            OperandInfo.implicitOperand(Accumulator.Word, destination)
      }

      def apply(immediate: ImmediateValue[?] & ByteSize, destination: Accumulator.LowByte.type): Static =
        Imm8ToAL(immediate)

      def apply(immediate: ImmediateValue[?] & ByteSize, destination: Accumulator.Word.type): Static =
        Imm8ToAX(immediate)

      def apply(port: Data.Word.type, destination: Accumulator.LowByte.type): Static =
        DXToAL()

      def apply(port: Data.Word.type, destination: Accumulator.Word.type): Static =
        DXToAX()
    }

    sealed trait I8086Output {
      val mnemonic: String = "out"

      private def ALToImm8(immediateValue: ImmediateValue[?] & ByteSize) =
        new Static(0xE6.toByte :: Nil, mnemonic) with NoDisplacement with Immediate[ByteSize](immediateValue, destination) {
          protected override def allOperands: Set[OperandInfo[?]] =
            super.allOperands + OperandInfo.implicitOperand(Accumulator.LowByte, source)
        }

      private def AXToImm8(immediateValue: ImmediateValue[?] & ByteSize) =
        new Static(0xE7.toByte :: Nil, mnemonic) with NoDisplacement with Immediate[ByteSize](immediateValue, destination) {
          protected override def allOperands: Set[OperandInfo[?]] =
            super.allOperands + OperandInfo.implicitOperand(Accumulator.Word, source)
        }

      private def ALToDX() =
        new Static(0xEE.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
          protected override def allOperands: Set[OperandInfo[?]] =
            super.allOperands +
              OperandInfo.implicitPort(Data.Word, destination) +
              OperandInfo.implicitOperand(Accumulator.LowByte, source)
        }

      private def AXToDX() =
        new Static(0xEF.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate  {
          protected override def allOperands: Set[OperandInfo[?]] =
            super.allOperands +
              OperandInfo.implicitPort(Data.Word, destination) +
              OperandInfo.implicitOperand(Accumulator.Word, source)
        }

      def apply(destination: Accumulator.LowByte.type, immediate: ImmediateValue[?] & ByteSize): Static & Immediate[ByteSize] =
        ALToImm8(immediate)

      def apply(destination: Accumulator.Word.type, immediate: ImmediateValue[?] & ByteSize): Static & Immediate[ByteSize] =
        AXToImm8(immediate)


      def apply(destination: Accumulator.LowByte.type, port: Data.Word.type): Static =
        ALToDX()

      def apply(destination: Accumulator.Word.type, port: Data.Word.type): Static =
        AXToDX()
    }
  }



  trait LegacyOperations extends Common {
    self: ProcessorMode.LegacyBounds & ProcessorMode & OperandSizeInfo =>

    object Input extends I8086Input
    object Output extends I8086Output
  }

  trait I386Operations extends Common {
    self: ArchitectureBounds & ProcessorMode & OperandSizeInfo =>

    sealed trait I386Input extends I8086Input {
      private def Imm8ToEAX(immediateValue: ImmediateValue[?] & ByteSize) =
        new Static(0xE5.toByte :: Nil, mnemonic) with NoDisplacement with Immediate[ByteSize](immediateValue, source) {
          protected override def allOperands: Set[OperandInfo[?]] =
            super.allOperands + OperandInfo.implicitOperand(Accumulator.DoubleWord, destination)
        }

      private def DXToEAX() = new Static(0xED.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[?]] =
          super.allOperands +
            OperandInfo.implicitPort(Data.Word, source) +
            OperandInfo.implicitOperand(Accumulator.DoubleWord, destination)
      }

      def apply(immediate: ImmediateValue[?] & ByteSize, destination: Accumulator.DoubleWord.type): Static =
        Imm8ToEAX(immediate)

      def apply(port: Data.Word.type, destination: Accumulator.DoubleWord.type): Static =
        DXToEAX()
    }


    sealed trait I386Output extends I8086Output {
      private def EAXToImm8(immediateValue: ImmediateValue[?] & ByteSize) =
        new Static(0xE7.toByte :: Nil, mnemonic) with NoDisplacement with Immediate[ByteSize](immediateValue, destination) {
          protected override def allOperands: Set[OperandInfo[?]] =
            super.allOperands + OperandInfo.implicitOperand(Accumulator.DoubleWord, source)
        }


      private def EAXToDX() =
        new Static(0xEF.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate  {
          protected override def allOperands: Set[OperandInfo[?]] =
            super.allOperands +
              OperandInfo.implicitPort(Data.Word, destination) +
              OperandInfo.implicitOperand(Accumulator.DoubleWord, source)
        }

      def apply(destination: Accumulator.DoubleWord.type, port: Data.Word.type): Static =
        EAXToDX()

      def apply(destination: Accumulator.DoubleWord.type, immediate: ImmediateValue[?] & ByteSize): Static & Immediate[ByteSize] =
        EAXToImm8(immediate)
    }

    object Input extends I386Input
    object Output extends I386Output
  }

  trait LongOperations extends I386Operations {
    self: ProcessorMode.LongBounds & ProcessorMode & OperandSizeInfo =>
  }

}
