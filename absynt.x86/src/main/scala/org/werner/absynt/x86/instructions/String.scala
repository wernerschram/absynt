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

import org.werner.absynt.x86.ArchitectureBounds
import org.werner.absynt.x86.operands._
import org.werner.absynt.x86.operands.memoryaccess.{DestinationReference, SourceReference}
import org.werner.absynt.x86.operands.registers.{AccumulatorRegister, Data}
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder
import org.werner.absynt.x86.operations._

object String {

  trait Operations {
    self: ArchitectureBounds with OperandSizeInfo =>

    protected def Static8(operand: Byte, mnemonic: String, staticOperands: Set[OperandInfo[_]]): X86Operation =
      new Static(operand :: Nil, mnemonic) with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands ++ staticOperands
      }

    protected def Static16[Size <: MaxWideSize](operand: Byte, mnemonic: String, staticOperands: Set[OperandInfo[_]]): X86Operation =
      new Static(operand :: Nil, mnemonic) with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands ++ staticOperands
      }

    protected def RepStatic8(opcode: Byte, mnemonic: String, staticOperands: Set[OperandInfo[_]]): X86Operation =
      new Static(opcode :: Nil, mnemonic) with NoDisplacement with NoImmediate with Repeated {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands ++ staticOperands
      }

    protected def RepStatic16[Size <: MaxWideSize](opcode: Byte, mnemonic: String, staticOperands: Set[OperandInfo[_]]): X86Operation =
      new Static(opcode :: Nil, mnemonic) with NoDisplacement with NoImmediate with Repeated {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands ++ staticOperands
      }

    protected def RepEStatic8(opcode: Byte, mnemonic: String, staticOperands: Set[OperandInfo[_]]): X86Operation =
      new Static(opcode :: Nil, mnemonic) with NoDisplacement with NoImmediate with RepeatEqual {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands ++ staticOperands
      }

    protected def RepEStatic16[Size <: MaxWideSize](opcode: Byte, mnemonic: String, staticOperands: Set[OperandInfo[_]]): X86Operation =
      new Static(opcode :: Nil, mnemonic) with NoDisplacement with NoImmediate with RepeatEqual {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands ++ staticOperands
      }

    protected def RepNEStatic8(opcode: Byte, mnemonic: String, staticOperands: Set[OperandInfo[_]]): X86Operation =
      new Static(opcode :: Nil, mnemonic) with NoDisplacement with NoImmediate with RepeatNotEqual {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands ++ staticOperands
      }

    protected def RepNEStatic16[Size <: MaxWideSize](opcode: Byte, mnemonic: String, staticOperands: Set[OperandInfo[_]]): X86Operation =
      new Static(opcode :: Nil, mnemonic) with NoDisplacement with NoImmediate with RepeatNotEqual {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands ++ staticOperands
      }

    object InString {
      private val byteOpcode = 0x6C.toByte
      private val wideOpcode = 0x6D.toByte
      private val mnemonic = "ins"

      private def operands[Size <: MaxValueSize](destination: DestinationReference[_] with Size): Set[OperandInfo[_]] = Set(
        OperandInfo.implicitAddress(destination, OperandOrder.destination),
        OperandInfo.implicitOperand(Data.Word, OperandOrder.source)(noOperandSizePrefixRequirement)
      )

      def apply[Size <: MaxValueSize](register: Data.Word.type, destination: DestinationReference[_] with Size): X86Operation =
        destination match {
          case _: DestinationReference[_] with ByteSize => Static8(byteOpcode, mnemonic, operands(destination))
          case _: DestinationReference[_] with MaxWideSize @unchecked => Static16(wideOpcode, mnemonic, operands(destination))
        }

      object Repeat {

        def apply[Size <: MaxValueSize](register: Data.Word.type, destination: DestinationReference[_] with Size): X86Operation =
          destination match {
            case _: DestinationReference[_] with ByteSize => RepStatic8(byteOpcode, mnemonic, operands(destination))
            case _: DestinationReference[_] with MaxWideSize @unchecked => RepStatic16(wideOpcode, mnemonic, operands(destination))
          }
      }
    }

    object MoveString {
      private val byteOpcode = 0xA4.toByte
      private val wideOpcode = 0xA5.toByte
      private val mnemonic = "movs"

      private def operands[Size <: MaxValueSize](
        source: SourceReference[_] with Size,
        destination: DestinationReference[_] with Size): Set[OperandInfo[_]] = Set(
        OperandInfo.implicitAddress(destination, OperandOrder.destination),
        OperandInfo.implicitAddress(source, OperandOrder.source),
      )

      def apply[Size <: MaxValueSize](
        register: SourceReference[_] with Size,
        destination: DestinationReference[_] with Size): X86Operation =
        destination match {
          case _: DestinationReference[_] with ByteSize => Static8(byteOpcode, mnemonic, operands(register, destination))
          case _: DestinationReference[_] with MaxWideSize @unchecked => Static16(wideOpcode, mnemonic, operands(register, destination))
        }

      object Repeat {

        def apply[Size <: MaxValueSize](
          register: SourceReference[_] with Size,
          destination: DestinationReference[_] with Size): X86Operation =
          destination match {
            case _: DestinationReference[_] with ByteSize =>
              RepStatic8(byteOpcode, mnemonic, operands(register, destination))
            case _: DestinationReference[_] with MaxWideSize @unchecked => RepStatic16(wideOpcode, mnemonic, operands(register, destination))
          }
      }
    }

    object OutString {
      private val byteOpcode = 0x6E.toByte
      private val wideOpcode = 0x6F.toByte
      private val mnemonic = "outs"

      private def operands[Size <: MaxValueSize](source: SourceReference[_] with Size): Set[OperandInfo[_]] = Set(
        OperandInfo.implicitOperand(Data.Word, OperandOrder.destination)(noOperandSizePrefixRequirement),
        OperandInfo.implicitAddress(source, OperandOrder.source),
      )

      def apply[Size <: MaxValueSize](source: SourceReference[_] with Size, register: Data.Word.type): X86Operation =
        source match {
          case _: SourceReference[_] with ByteSize => Static8(byteOpcode, mnemonic, operands(source))
          case _: SourceReference[_] with MaxWideSize @unchecked => Static16(wideOpcode, mnemonic, operands(source))
        }

      object Repeat {

        def apply[Size <: MaxValueSize](source: SourceReference[_] with Size, register: Data.Word.type): X86Operation =
          source match {
            case _: SourceReference[_] with ByteSize => RepStatic8(byteOpcode, mnemonic, operands(source))
            case _: SourceReference[_] with MaxWideSize @unchecked => RepStatic16(wideOpcode, mnemonic, operands(source))
          }
      }
    }

    object LoadString {
      private val byteOpcode = 0xAC.toByte
      private val wideOpcode = 0xAD.toByte
      private val mnemonic = "lods"

      private def operands[Size <: MaxValueSize](
        source: SourceReference[_] with Size,
        destination: AccumulatorRegister with Size): Set[OperandInfo[_]] = Set(
        OperandInfo.implicitAddress(source, OperandOrder.source),
        OperandInfo.implicitOperand(destination, OperandOrder.destination)
      )

      def apply[Size <: MaxValueSize](source: SourceReference[_] with Size, register: AccumulatorRegister with Size): X86Operation =
        source match {
          case _: SourceReference[_] with ByteSize => Static8(byteOpcode, mnemonic, operands(source, register))
          case _: SourceReference[_] with MaxWideSize @unchecked => Static16(wideOpcode, mnemonic, operands(source, register))
        }

      object Repeat {

        def apply[Size <: MaxValueSize](
          source: SourceReference[_] with Size,
          register: AccumulatorRegister with Size): X86Operation =
          source match {
            case _: SourceReference[_] with ByteSize => RepStatic8(byteOpcode, mnemonic, operands(source, register))
            case _: SourceReference[_] with MaxWideSize @unchecked => RepStatic16(wideOpcode, mnemonic, operands(source, register))
          }
      }
    }

    object StoreString {
      private val byteOpcode = 0xAA.toByte
      private val wideOpcode = 0xAB.toByte
      private val mnemonic = "stos"

      private def operands[Size <: MaxValueSize](
        source: AccumulatorRegister with Size,
        destination: DestinationReference[_] with Size): Set[OperandInfo[_]] = Set(
        OperandInfo.implicitAddress(destination, OperandOrder.destination),
        OperandInfo.implicitOperand(source, OperandOrder.source)
      )

      def apply[Size <: MaxValueSize](
        register: AccumulatorRegister with Size,
        destination: DestinationReference[_] with Size): X86Operation =
        destination match {
          case _: DestinationReference[_] with ByteSize => Static8(byteOpcode, mnemonic, operands(register, destination))
          case _: DestinationReference[_] with MaxWideSize @unchecked => Static16(wideOpcode, mnemonic, operands(register, destination))
        }

      object Repeat {

        def apply[Size <: MaxValueSize](
          register: AccumulatorRegister with Size,
          destination: DestinationReference[_] with Size): X86Operation =
          destination match {
            case _: DestinationReference[_] with ByteSize =>
              RepStatic8(byteOpcode, mnemonic, operands(register, destination))
            case _: DestinationReference[_] with MaxWideSize @unchecked => RepStatic16(wideOpcode, mnemonic, operands(register, destination))
          }
      }
    }

    object CompareString {
      private val byteOpcode = 0xA6.toByte
      private val wideOpcode = 0xA7.toByte
      private val mnemonic = "cmps"

      private def operands[Size <: MaxValueSize](
        source: SourceReference[_] with Size,
        destination: DestinationReference[_] with Size): Set[OperandInfo[_]] = Set(
        OperandInfo.implicitAddress(destination, OperandOrder.destination),
        OperandInfo.implicitOperand(source, OperandOrder.source)
      )

      def apply[Size <: MaxValueSize](
        source: SourceReference[_] with Size,
        destination: DestinationReference[_] with Size): X86Operation =
        destination match {
          case _: DestinationReference[_] with ByteSize => Static8(byteOpcode, mnemonic, operands(source, destination))
          case _: DestinationReference[_] with MaxWideSize @unchecked => Static16(wideOpcode, mnemonic, operands(source, destination))
        }

      object RepeatEqual {

        def apply[Size <: MaxValueSize](
          source: SourceReference[_] with Size,
          destination: DestinationReference[_] with Size): X86Operation =
          destination match {
            case _: DestinationReference[_] with ByteSize =>
              RepEStatic8(byteOpcode, mnemonic, operands(source, destination))
            case _: DestinationReference[_] with MaxWideSize @unchecked => RepEStatic16(wideOpcode, mnemonic, operands(source, destination))
          }
      }

      object RepeatNotEqual {

        def apply[Size <: MaxValueSize](
          source: SourceReference[_] with Size,
          destination: DestinationReference[_] with Size): X86Operation =
          destination match {
            case _: DestinationReference[_] with ByteSize =>
              RepNEStatic8(byteOpcode, mnemonic, operands(source, destination))
            case _: DestinationReference[_] with MaxWideSize @unchecked => RepNEStatic16(wideOpcode, mnemonic, operands(source, destination))
          }
      }
    }

    object ScanString {
      private val byteOpcode = 0xAE.toByte
      private val wideOpcode = 0xAF.toByte
      private val mnemonic = "sca" +
        "s"

      private def operands[Size <: MaxValueSize](
        source: AccumulatorRegister with Size,
        destination: DestinationReference[_] with Size): Set[OperandInfo[_]] = Set(
        OperandInfo.implicitAddress(destination, OperandOrder.destination),
        OperandInfo.implicitOperand(source, OperandOrder.source)
      )

      def apply[Size <: MaxValueSize](
        source: AccumulatorRegister with Size,
        destination: DestinationReference[_] with Size): X86Operation =
        destination match {
          case _: DestinationReference[_] with ByteSize => Static8(byteOpcode, mnemonic, operands(source, destination))
          case _: DestinationReference[_] with MaxWideSize @unchecked => Static16(wideOpcode, mnemonic, operands(source, destination))
        }

      object RepeatEqual {

        def apply[Size <: MaxValueSize](
          source: AccumulatorRegister with Size,
          destination: DestinationReference[_] with Size): X86Operation =
          destination match {
            case _: DestinationReference[_] with ByteSize =>
              RepEStatic8(byteOpcode, mnemonic, operands(source, destination))
            case _: DestinationReference[_] with MaxWideSize @unchecked => RepEStatic16(wideOpcode, mnemonic, operands(source, destination))
          }
      }

      object RepeatNotEqual {

        def apply[Size <: MaxValueSize](
          source: AccumulatorRegister with Size,
          destination: DestinationReference[_] with Size): X86Operation =
          destination match {
            case _: DestinationReference[_] with ByteSize =>
              RepNEStatic8(byteOpcode, mnemonic, operands(source, destination))
            case _: DestinationReference[_] with MaxWideSize @unchecked => RepNEStatic16(wideOpcode, mnemonic, operands(source, destination))
          }
      }
    }
  }
}
