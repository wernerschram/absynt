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
import org.werner.absynt.x86.operands.registers.{GeneralPurposeRegister, Segment, SegmentRegister}
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder._
import org.werner.absynt.x86.operations._

object Stack {

  sealed trait PushCommon {
    self: ArchitectureBounds with OperandSizeInfo =>

    type RMMaxSize <: MaxWideSize
    type ImmMaxSize <: MaxValueSize
    type ImmExtendedMaxSize <: MaxWideSize

    sealed trait PushOperations {
      private val mnemonic: String = "push"

      protected def StaticCS(): X86Operation = new Static(0x0E.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Segment.Code, OperandOrder.source)(noOperandSizePrefixRequirement)
      }
      protected def StaticSS(): X86Operation = new Static(0x16.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Segment.Stack, OperandOrder.source)(noOperandSizePrefixRequirement)
      }
      protected def StaticDS(): X86Operation = new Static(0x1E.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Segment.Data, OperandOrder.source)(noOperandSizePrefixRequirement)
      }
      protected def StaticES(): X86Operation = new Static(0x06.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Segment.Extra, OperandOrder.source)(noOperandSizePrefixRequirement)
      }
      protected def StaticFS(): X86Operation = new Static(0x0F.toByte :: 0xA0.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Segment.MoreExtra, OperandOrder.source)(noOperandSizePrefixRequirement)
      }
      protected def StaticGS(): X86Operation = new Static(0x0F.toByte :: 0xA8.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Segment.StillMoreExtra, OperandOrder.source)(noOperandSizePrefixRequirement)
      }

      protected def PrefStaticCS(): X86Operation = new Static(0x0E.toByte :: Nil, s"${mnemonic}w") with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Segment.Code, OperandOrder.source)(alwaysOperandSizePrefixRequirement)
      }
      protected def PrefStaticSS(): X86Operation = new Static(0x16.toByte :: Nil, s"${mnemonic}w") with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Segment.Stack, OperandOrder.source)(alwaysOperandSizePrefixRequirement)
      }
      protected def PrefStaticDS(): X86Operation = new Static(0x1E.toByte :: Nil, s"${mnemonic}w") with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Segment.Data, OperandOrder.source)(alwaysOperandSizePrefixRequirement)
      }
      protected def PrefStaticES(): X86Operation = new Static(0x06.toByte :: Nil, s"${mnemonic}w") with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Segment.Extra, OperandOrder.source)(alwaysOperandSizePrefixRequirement)
      }
      protected def PrefStaticFS(): X86Operation = new Static(0x0F.toByte :: 0xA0.toByte :: Nil, s"${mnemonic}w") with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Segment.MoreExtra, OperandOrder.source)(alwaysOperandSizePrefixRequirement)
      }
      protected def PrefStaticGS(): X86Operation = new Static(0x0F.toByte :: 0xA8.toByte :: Nil, s"${mnemonic}w") with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Segment.StillMoreExtra, OperandOrder.source)(alwaysOperandSizePrefixRequirement)
      }

      protected def Imm16[Size <: ImmExtendedMaxSize](immediateValue: ImmediateValue[_] with Size): X86Operation =
        new Static(0x68.toByte :: Nil, mnemonic) with NoDisplacement with Immediate[Size] {
          override def immediate: OperandWithOperandSizePrefixInfo[ImmediateValue[_] with Size] = immediateValue
          override def immediateOrder: OperandOrder = destination
        }

      protected def R16[Size <: RMMaxSize](register: GeneralPurposeRegister with Size): X86Operation =
        new RegisterEncoded[RMMaxSize](register, Seq(0x50.toByte), mnemonic) with NoDisplacement with NoImmediate {
          override def registerOrder: OperandOrder = destination
        }

      protected def RM16(operand: ModRMEncodableOperand with RMMaxSize) =
        new ModRM(operand, 0xFF.toByte :: Nil, 0x06.toByte, mnemonic, destination) with NoDisplacement with NoImmediate


      protected def Imm8(immediateValue: ImmediateValue[_] with ByteSize): X86Operation =
        new Static(0x6A.toByte :: Nil, mnemonic) with NoDisplacement with Immediate[ByteSize] {
          override def immediate: OperandWithOperandSizePrefixInfo[ImmediateValue[_] with ByteSize] = immediateValue
          override def immediateOrder: OperandOrder = destination
        }

      def apply[Size <: RMMaxSize](register: GeneralPurposeRegister with Size): X86Operation =
        R16(register)

      def apply[Size <: RMMaxSize](operand: ModRMEncodableOperand with Size): X86Operation =
        RM16(operand)

      def apply(immediate: ImmediateValue[_] with ImmMaxSize): X86Operation =
        immediate match {
          case i: ByteSize => Imm8(i)
          case i: ImmExtendedMaxSize @unchecked => Imm16(i)
        }
    }

    object PushFlags {
      implicit val opcode: String = "pushf"

      def apply(): Static = new Static(0x9C.toByte :: Nil, opcode) with NoDisplacement with NoImmediate
    }

    sealed trait PushAllOperations {
      implicit val opcode: String = "pusha"

      def apply(): Static =
        new Static(0x60.toByte :: Nil, opcode) with NoDisplacement with NoImmediate
    }
  }

  sealed trait PopCommon {
    self: ArchitectureBounds with OperandSizeInfo =>

    private val mnemonic: String = "pop"

    type RMMaxSize <: MaxWideSize

    sealed class PopOperations {
      protected def RM16(operand: ModRMEncodableOperand with RMMaxSize) =
        new ModRM(operand, 0x8F.toByte :: Nil, 0x00.toByte, mnemonic, destination) with NoDisplacement with NoImmediate

      protected def R16[Size <: RMMaxSize](register: GeneralPurposeRegister with Size): X86Operation =
        new RegisterEncoded[RMMaxSize](register, Seq(0x58.toByte), mnemonic) with NoDisplacement with NoImmediate {
          override def registerOrder: OperandOrder = destination
        }

      protected def StaticSS(): X86Operation = new Static(0x17.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Segment.Stack, OperandOrder.source)(noOperandSizePrefixRequirement)
      }

      protected def StaticDS(): X86Operation = new Static(0x1F.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Segment.Data, OperandOrder.source)(noOperandSizePrefixRequirement)
      }

      protected def StaticES(): X86Operation = new Static(0x07.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Segment.Extra, OperandOrder.source)(noOperandSizePrefixRequirement)
      }

      protected def StaticFS(): X86Operation = new Static(0x0F.toByte :: 0xA1.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Segment.MoreExtra, OperandOrder.source)(noOperandSizePrefixRequirement)
      }

      protected def StaticGS(): X86Operation = new Static(0x0F.toByte :: 0xA9.toByte :: Nil, mnemonic) with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Segment.StillMoreExtra, OperandOrder.source)(noOperandSizePrefixRequirement)
      }

      protected def PrefStaticSS(): X86Operation = new Static(0x17.toByte :: Nil, s"${mnemonic}w") with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Segment.Stack, OperandOrder.source)(alwaysOperandSizePrefixRequirement)
      }

      protected def PrefStaticDS(): X86Operation = new Static(0x1F.toByte :: Nil, s"${mnemonic}w") with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Segment.Data, OperandOrder.source)(alwaysOperandSizePrefixRequirement)
      }

      protected def PrefStaticES(): X86Operation = new Static(0x07.toByte :: Nil, s"${mnemonic}w") with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Segment.Extra, OperandOrder.source)(alwaysOperandSizePrefixRequirement)
      }

      protected def PrefStaticFS(): X86Operation = new Static(0x0F.toByte :: 0xA1.toByte :: Nil, s"${mnemonic}w") with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Segment.MoreExtra, OperandOrder.source)(alwaysOperandSizePrefixRequirement)
      }

      protected def PrefStaticGS(): X86Operation = new Static(0x0F.toByte :: 0xA9.toByte :: Nil, s"${mnemonic}w") with NoDisplacement with NoImmediate {
        protected override def allOperands: Set[OperandInfo[_]] =
          super.allOperands + OperandInfo.implicitOperand(Segment.StillMoreExtra, OperandOrder.source)(alwaysOperandSizePrefixRequirement)
      }

      def apply(operand: ModRMEncodableOperand with RMMaxSize): X86Operation =
        RM16(operand)

      def apply[Size <: RMMaxSize](register: GeneralPurposeRegister with Size): X86Operation =
        R16(register)
    }

    object PopFlags {
      implicit val opcode: String = "popf"

      def apply(): Static = new Static(0x9D.toByte :: Nil, opcode) with NoDisplacement with NoImmediate
    }

    sealed trait PopAllOperations {
      implicit val opcode: String = "popa"

      def apply(): Static =
        new Static(0x61.toByte :: Nil, opcode) with NoDisplacement with NoImmediate
    }

  }

  trait LegacyOperations extends PushCommon with PopCommon {
    self: ProcessorMode.LegacyBounds with OperandSizeInfo  =>

    override type RMMaxSize = WordSize
    override type ImmMaxSize = ByteWordSize
    override type ImmExtendedMaxSize = WordSize

    object Push extends PushOperations {
      def apply(segment: SegmentRegister): X86Operation = segment match {
        case Segment.Code => StaticCS()
        case Segment.Stack => StaticSS()
        case Segment.Data => StaticDS()
        case Segment.Extra => StaticES()
        case _ => throw new AssertionError()
      }
    }

    object PushAll extends PushAllOperations

    object Pop extends PopOperations {
      def apply(segment: SegmentRegister): X86Operation = segment match {
        case Segment.Stack => StaticSS()
        case Segment.Data => StaticDS()
        case Segment.Extra => StaticES()
        case _ => throw new AssertionError()
      }
    }

    object PopAll extends PopAllOperations
  }

  trait I386Operations extends PushCommon with PopCommon {
    self: ProcessorMode.I386Bounds with OperandSizeInfo  =>

    override type RMMaxSize = WordDoubleSize
    override type ImmMaxSize = ByteWordDoubleSize
    override type ImmExtendedMaxSize = WordDoubleSize

    object Push extends PushOperations {
      def apply(segment: SegmentRegister): X86Operation = segment match {
        case Segment.Code => StaticCS()
        case Segment.Stack => StaticSS()
        case Segment.Data => StaticDS()
        case Segment.Extra => StaticES()
        case Segment.MoreExtra => StaticFS()
        case Segment.StillMoreExtra => StaticGS()
      }

      object Unaligned {
        def apply(segment: SegmentRegister): X86Operation = segment match {
          case Segment.Code => PrefStaticCS()
          case Segment.Stack => PrefStaticSS()
          case Segment.Data => PrefStaticDS()
          case Segment.Extra => PrefStaticES()
          case Segment.MoreExtra => PrefStaticFS()
          case Segment.StillMoreExtra => PrefStaticGS()
        }
      }
    }

    object PushAll extends PushAllOperations

    object Pop extends PopOperations {
      def apply(segment: SegmentRegister): X86Operation = segment match {
        case Segment.Stack => StaticSS()
        case Segment.Data => StaticDS()
        case Segment.Extra => StaticES()
        case Segment.MoreExtra => StaticFS()
        case Segment.StillMoreExtra => StaticGS()
        case _ => throw new AssertionError()
      }
      object Unaligned {
        def apply(segment: SegmentRegister): X86Operation = segment match {
          case Segment.Stack => PrefStaticSS()
          case Segment.Data => PrefStaticDS()
          case Segment.Extra => PrefStaticES()
          case Segment.MoreExtra => PrefStaticFS()
          case Segment.StillMoreExtra => PrefStaticGS()
          case _ => throw new AssertionError()
        }
      }
    }

    object PopAll extends PopAllOperations
  }

  trait LongOperations extends PushCommon with PopCommon {
    self: ProcessorMode.LongBounds with OperandSizeInfo =>

    type RMMaxSize = WordQuadSize
    override type ImmMaxSize = ByteWordDoubleSize
    override type ImmExtendedMaxSize = WordDoubleSize

    object Push extends PushOperations {
      def apply(segment: SegmentRegister): X86Operation = segment match {
        case Segment.MoreExtra => StaticFS()
        case Segment.StillMoreExtra => StaticGS()
        case _ => throw new AssertionError()
      }
    }

    object Pop extends PopOperations {
      def apply(segment: SegmentRegister): X86Operation = segment match {
        case Segment.MoreExtra => StaticFS()
        case Segment.StillMoreExtra => StaticGS()
        case _ => throw new AssertionError()
      }
    }
  }
}


