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

import org.werner.absynt.x86.{ArchitectureBounds, ProcessorMode}
import org.werner.absynt.x86.operands._
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder._
import org.werner.absynt.x86.operations._

object Stack {

  sealed trait Common {
    self: ArchitectureBounds =>

    type RMMaxSize <: MaxWideSize
    type ImmMaxSize <: MaxValueSize
    type ImmExtendedMaxSize <: MaxWideSize

    sealed trait PushOperations {
      private val mnemonic: String = "push"

      private def StaticCS() = new Static(0x0E.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate
      private def StaticSS() = new Static(0x16.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate
      private def StaticDS() = new Static(0x1E.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate
      private def StaticES() = new Static(0x06.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate
      private def StaticFS() = new Static(0x0F.toByte :: 0xA0.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate
      private def StaticGS() = new Static(0x0F.toByte :: 0xA8.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate

      protected def Imm16[Size <: ImmExtendedMaxSize](immediateValue: ImmediateValue with Size): X86Operation =
        new Static(0x68.toByte :: Nil, mnemonic) with NoDisplacement with Immediate[Size] {
          override def immediate: OperandWithOperandSizePrefixInfo[ImmediateValue with Size] = immediateValue
          override def immediateOrder: OperandOrder = destination
        }

      protected def R16[Size <: RMMaxSize](register: GeneralPurposeRegister with Size): X86Operation =
        new RegisterEncoded[RMMaxSize](register, Seq(0x50.toByte), mnemonic) with NoDisplacement with NoImmediate {
          override def registerOrder: OperandOrder = destination
        }

      protected def RM16(operand: ModRMEncodableOperand with RMMaxSize) =
        new ModRM(operand, 0xFF.toByte :: Nil, 0x06.toByte, mnemonic, destination) with NoDisplacement with NoImmediate


      protected def Imm8(immediateValue: ImmediateValue with ByteSize): X86Operation =
        new Static(0x6A.toByte :: Nil, mnemonic) with NoDisplacement with Immediate[ByteSize] {
          override def immediate: OperandWithOperandSizePrefixInfo[ImmediateValue with ByteSize] = immediateValue
          override def immediateOrder: OperandOrder = destination
        }

      def apply(segment: SegmentRegister): Static = segment match {
        case Segment.Code => StaticCS()
        case Segment.Stack => StaticSS()
        case Segment.Data => StaticDS()
        case Segment.Extra => StaticES()
        case Segment.MoreExtra => StaticFS()
        case Segment.StillMoreExtra => StaticGS()
      }

      def apply[Size <: RMMaxSize](register: GeneralPurposeRegister with Size): X86Operation =
        R16(register)

      def apply[Size <: RMMaxSize](operand: ModRMEncodableOperand with Size): X86Operation =
        RM16(operand)

      def apply(immediate: ImmediateValue with ImmMaxSize): X86Operation =
        immediate match {
          case i: ByteSize => Imm8(i)
          case i: ImmExtendedMaxSize @unchecked => Imm16(i)
        }
    }
  }

  trait LegacyOperations extends Common {
    self: ProcessorMode.LegacyBounds =>

    override type RMMaxSize = WordSize
    override type ImmMaxSize = ByteWordSize
    override type ImmExtendedMaxSize = WordSize

    object Push extends PushOperations

    object PushAll {
      implicit val opcode: String = "pusha"

      def apply(): Static =
        new Static(0x60.toByte :: Nil, opcode) with NoDisplacement with NoImmediate
    }

    object PushFlags {
      implicit val opcode: String = "pushf"

      def apply(): Static = new Static(0x9C.toByte :: Nil, opcode) with NoDisplacement with NoImmediate
    }

  }

  trait RealOperations extends Common {
    self: ProcessorMode.RealBounds =>

    override type RMMaxSize = WordDoubleSize
    override type ImmMaxSize = ByteWordDoubleSize
    override type ImmExtendedMaxSize = WordDoubleSize

    object Push extends PushOperations

    object PushAll {
      implicit val opcode: String = "pusha"

      def apply(): Static =
        new Static(0x60.toByte :: Nil, opcode) with NoDisplacement with NoImmediate
    }

    object PushFlags {
      implicit val opcode: String = "pushf"

      def apply(): Static =
        new Static(0x9C.toByte :: Nil, opcode) with NoDisplacement with NoImmediate
    }
  }

  trait ProtectedOperations extends Common {
    self: ProcessorMode.ProtectedBounds =>

    override type RMMaxSize = WordDoubleSize
    override type ImmMaxSize = ByteWordDoubleSize
    override type ImmExtendedMaxSize = WordDoubleSize

    object Push extends PushOperations

    object PushAll {
      implicit val opcode: String = "pusha"

      def apply(): Static =
        new Static(0x60.toByte :: Nil, opcode) with NoDisplacement with NoImmediate
    }

    object PushFlags {
      implicit val opcode: String = "pushf"

      def apply(): Static =
        new Static(0x9C.toByte :: Nil, opcode) with NoDisplacement with NoImmediate
    }
  }

  trait LongOperations extends Common {
    self: ProcessorMode.LongBounds =>

    type RMMaxSize = WordQuadSize
    override type ImmMaxSize = ByteWordDoubleSize
    override type ImmExtendedMaxSize = WordDoubleSize

    object Push extends PushOperations

    object PushFlags {
      implicit val opcode: String = "pushf"

      def apply(): Static =
        new Static(0x9C.toByte :: Nil, opcode) with NoDisplacement with NoImmediate
    }
  }
}


