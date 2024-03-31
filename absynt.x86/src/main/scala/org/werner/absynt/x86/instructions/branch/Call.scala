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

package org.werner.absynt.x86.instructions.branch

import org.werner.absynt.Label
import org.werner.absynt.resource.{RelativeReference, Resource, UnlabeledEncodable}
import org.werner.absynt.x86._
import org.werner.absynt.x86.operands._
import org.werner.absynt.x86.operands.memoryaccess._
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder._
import org.werner.absynt.x86.operations.branch.{JumpOption, LabelJumpOperation}
import org.werner.absynt.x86.operations.{Immediate, ModRM, NoDisplacement, NoImmediate, OperandSizeInfo, OperandSizePrefixRequirement, OperandWithOperandSizePrefixInfo, Static, X86Operation, FarPointer => FarPointerOperation, NearPointer => NearPointerOperation}

object Call {

  sealed trait CallOperations {
    self: ArchitectureBounds with OperandSizeInfo =>

    sealed trait BaseCall {

      val mnemonic = "call"
      val opcode: Seq[Byte] = Seq(0xe8.toByte)

      protected def Rel16(nearPointer: NearPointer with WordSize): Static with NearPointerOperation[WordSize] with NoImmediate = {
        new Static(opcode, mnemonic) with NearPointerOperation[WordSize] with NoImmediate {
          override val pointer: OperandWithOperandSizePrefixInfo[NearPointer with WordSize] = nearPointer

          override def pointerOrder: OperandOrder = destination
        }
      }

      protected def Rel32(nearPointer: NearPointer with DoubleWordSize): Static with NearPointerOperation[DoubleWordSize] with NoImmediate = {
        new Static(opcode, mnemonic) with NearPointerOperation[DoubleWordSize] with NoImmediate {
          override val pointer: OperandWithOperandSizePrefixInfo[NearPointer with DoubleWordSize] = nearPointer

          override def pointerOrder: OperandOrder = destination
        }
      }

      protected def RM16[Size <: WordDoubleQuadSize](operand: ModRMEncodableOperand with Size): ModRM[ModRMEncodableOperand with Size] with NoDisplacement with NoImmediate =
        new ModRM(operand, 0xff.toByte :: Nil, 2, mnemonic, destination, false) with NoDisplacement with NoImmediate

      protected def Ptr1616[Size <: WordDoubleSize](farPointer: FarPointer[Size] with FarPointerSize[Size]): Static with FarPointerOperation[Size] with NoImmediate =
        new Static(0x9A.toByte :: Nil, mnemonic) with FarPointerOperation[Size] with NoImmediate {
          override def pointer: OperandWithOperandSizePrefixInfo[FarPointer[Size] with FarPointerSize[Size]] = farPointer
        }

      protected def M1616(operand: MemoryLocation with WordDoubleQuadSize): ModRM[MemoryLocation with WordDoubleQuadSize] with NoDisplacement with NoImmediate =
        new ModRM(operand, 0xFF.toByte :: Nil, 3, s"$mnemonic FAR", destination) with NoDisplacement with NoImmediate
    }

  }

  sealed trait ReturnOperations {
    self: ArchitectureBounds with OperandSizeInfo =>

    object Return {

      val mnemonic: String = "ret"

      private def Static =
        new Static(0xC3.toByte :: Nil, "ret") with NoDisplacement with NoImmediate

      protected def Imm16(immediateValue: ImmediateValue[_] with WordSize): X86Operation =
        new Static(0xC2.toByte :: Nil, "ret") with NoDisplacement with Immediate[WordSize] {
          override def immediate: OperandWithOperandSizePrefixInfo[ImmediateValue[_] with WordSize] = operandWithOperandSizePrefixInfo(immediateValue)(noOperandSizePrefixRequirement)

          override def immediateOrder: OperandOrder = source
        }


      def apply(): X86Operation = Static

      def apply(immediateValue: ImmediateValue[_] with WordSize): X86Operation = Imm16(immediateValue)

      object Far {
        private def Static =
          new Static(0xCB.toByte :: Nil, "retf") with NoDisplacement with NoImmediate

        protected def Imm16(immediateValue: ImmediateValue[_] with WordSize): X86Operation =
          new Static(0xCA.toByte :: Nil, "retf") with NoDisplacement with Immediate[WordSize] {
            implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = noOperandSizePrefixRequirement

            override def immediate: OperandWithOperandSizePrefixInfo[ImmediateValue[_] with WordSize] = immediateValue

            override def immediateOrder: OperandOrder = source
          }

        def apply(): X86Operation = Static

        def apply(immediateValue: ImmediateValue[_] with WordSize): X86Operation = Imm16(immediateValue)
      }

    }

  }

  trait LegacyOperations extends ReturnOperations with CallOperations {
    self: ProcessorMode.LegacyBounds with OperandSizeInfo =>

    object Call extends BaseCall {

      def apply(nearPointer: NearPointer with WordSize): X86Operation =
        Rel16(nearPointer)

      def apply[Size <: WordDoubleSize](operand: ModRMEncodableOperand with Size): X86Operation =
        RM16(operand)

      object Far {
        def apply(farPointer: FarPointer[WordSize] with FarPointerSize[WordSize]): Static with FarPointerOperation[WordSize] =
          Ptr1616(farPointer)

        def apply(pointer: MemoryLocation with WordSize): X86Operation =
          M1616(pointer)
      }

      def apply(targetLabel: Label): RelativeReference = {
        LabelJumpOperation(
          Seq(new JumpOption(3, Short.MinValue, Short.MaxValue) {
            override def encodableForPointer(offset: Int): Resource with UnlabeledEncodable =
              Rel16(LongPointer.realMode(offset))
          }),
          mnemonic,
          targetLabel
        )
      }
    }
  }

  trait RealOperations extends ReturnOperations with CallOperations {
    self: ProcessorMode.I386Bounds with OperandSizeInfo =>

    object Call extends BaseCall {

      def apply(nearPointer: NearPointer with WordDoubleSize): X86Operation =
        nearPointer match {
          case p: NearPointer with WordSize =>
            Rel16(p)
          case p: NearPointer with DoubleWordSize =>
            Rel32(p)
        }

      def apply[Size <: WordDoubleSize](operand: ModRMEncodableOperand with Size): X86Operation =
        RM16(operand)

      object Far {
        def apply[Size <: WordDoubleSize](farPointer: FarPointer[Size] with FarPointerSize[Size]): Static with FarPointerOperation[Size] =
          Ptr1616(farPointer)

        def apply(pointer: MemoryLocation with WordDoubleSize): X86Operation =
          M1616(pointer)
      }

      def apply(targetLabel: Label): RelativeReference = {
        LabelJumpOperation(
          Seq(
            new JumpOption(3, Short.MinValue, Short.MaxValue) {
              override def encodableForPointer(offset: Int): Resource with UnlabeledEncodable =
                Rel16(LongPointer.realMode(offset))
            },
            new JumpOption(6, Int.MinValue, Int.MaxValue) {
              override def encodableForPointer(offset: Int): Resource with UnlabeledEncodable =
                Rel32(LongPointer.protectedMode(offset))
            }
          ),
          mnemonic,
          targetLabel
        )
      }
    }
  }

  trait ProtectedOperations extends ReturnOperations with CallOperations {
    self: ProcessorMode.I386Bounds with OperandSizeInfo =>

    object Call extends BaseCall {

      def apply(nearPointer: NearPointer with WordDoubleSize): X86Operation =
        nearPointer match {
          case p: NearPointer with WordSize =>
            Rel16(p)
          case p: NearPointer with DoubleWordSize =>
            Rel32(p)
        }

      def apply[Size <: WordDoubleSize](operand: ModRMEncodableOperand with Size): X86Operation =
        RM16(operand)

      object Far {
        def apply[Size <: WordDoubleSize](farPointer: FarPointer[Size] with FarPointerSize[Size]): Static with FarPointerOperation[Size] =
          Ptr1616(farPointer)

        def apply(pointer: MemoryLocation with WordDoubleSize): X86Operation =
          M1616(pointer)
      }

      def apply(targetLabel: Label): RelativeReference = {
        LabelJumpOperation(
          Seq(
            new JumpOption(4, Short.MinValue, Short.MaxValue) {
              override def encodableForPointer(offset: Int): Resource with UnlabeledEncodable =
                Rel16(LongPointer.realMode(offset))
            },
            new JumpOption(5, Int.MinValue, Int.MaxValue) {
              override def encodableForPointer(offset: Int): Resource with UnlabeledEncodable =
                Rel32(LongPointer.protectedMode(offset))
            }
          ),
          mnemonic,
          targetLabel
        )
      }
    }
  }

  trait LongOperations extends ReturnOperations with CallOperations {
    self: ProcessorMode.LongBounds with OperandSizeInfo =>

    object Call extends BaseCall {

      def apply(nearPointer: NearPointer with DoubleWordSize): X86Operation =
        Rel32(nearPointer)

      def apply(operand: ModRMEncodableOperand with QuadWordSize): X86Operation =
        RM16(operand)

      object Far {
        def apply[Size <: MaxWideSize](pointer: MemoryLocation with Size): X86Operation =
          M1616(pointer)
      }

      def apply(targetLabel: Label): RelativeReference = {
        LabelJumpOperation(
          Seq(
            new JumpOption(5, Int.MinValue, Int.MaxValue) {
              override def encodableForPointer(offset: Int): Resource with UnlabeledEncodable =
                Rel32(LongPointer.protectedMode(offset))
            }
          ),
          mnemonic,
          targetLabel
        )
      }
    }
  }
}
