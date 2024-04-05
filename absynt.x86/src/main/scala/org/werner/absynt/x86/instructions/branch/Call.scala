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
import org.werner.absynt.x86.*
import org.werner.absynt.x86.operands.*
import org.werner.absynt.x86.operands.memoryaccess.*
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder.*
import org.werner.absynt.x86.operations.branch.{JumpOption, LabelJumpOperation}
import org.werner.absynt.x86.operations.{Immediate, ModRM, NoDisplacement, NoImmediate, OperandSizeInfo, OperandWithOperandSizePrefixInfo, Static, X86Operation, FarPointer as FarPointerOperation, NearPointer as NearPointerOperation}
import scala.language.implicitConversions

object Call {

  sealed trait CallOperations {
    self: ArchitectureBounds & ProcessorMode & OperandSizeInfo =>

    sealed trait BaseCall {

      val mnemonic = "call"
      val opcode: Seq[Byte] = Seq(0xe8.toByte)

      protected def Rel16(nearPointer: NearPointer & WordSize): Static & NearPointerOperation[WordSize] & NoImmediate = 
        new Static(opcode, mnemonic) 
          with NearPointerOperation[WordSize](nearPointer, destination) 
          with NoImmediate 

      protected def Rel32(nearPointer: NearPointer & DoubleWordSize): Static & NearPointerOperation[DoubleWordSize] & NoImmediate =
        new Static(opcode, mnemonic) 
          with NearPointerOperation[DoubleWordSize](nearPointer, destination) 
          with NoImmediate

      protected def RM16[Size <: WordDoubleQuadSize](operand: ModRMEncodableOperand & Size): ModRM[ModRMEncodableOperand & Size] & NoDisplacement & NoImmediate =
        new ModRM(operand, 0xff.toByte :: Nil, 2, mnemonic, destination, false) 
          with NoDisplacement 
          with NoImmediate

      protected def Ptr1616[Size <: WordDoubleSize](farPointer: FarPointer[Size] & FarPointerSize[Size]): Static & FarPointerOperation[Size] & NoImmediate =
        new Static(0x9A.toByte :: Nil, mnemonic) 
          with FarPointerOperation[Size](farPointer)
          with NoImmediate

      protected def M1616(operand: MemoryLocation & WordDoubleQuadSize): ModRM[MemoryLocation & WordDoubleQuadSize] & NoDisplacement & NoImmediate =
        new ModRM(operand, 0xFF.toByte :: Nil, 3, s"$mnemonic FAR", destination) 
          with NoDisplacement 
          with NoImmediate
    }

  }

  sealed trait ReturnOperations {
    self: ArchitectureBounds & ProcessorMode & OperandSizeInfo =>

    object Return {

      val mnemonic: String = "ret"

      private def Static =
        new Static(0xC3.toByte :: Nil, "ret") with NoDisplacement with NoImmediate

      protected def Imm16(immediateValue: ImmediateValue[?] & WordSize): X86Operation =
        new Static(0xC2.toByte :: Nil, "ret") with NoDisplacement with Immediate[WordSize](immediateValue.withSizePrefixRequirement(noOperandSizePrefixRequirement), source)


      def apply(): X86Operation = Static

      def apply(immediateValue: ImmediateValue[?] & WordSize): X86Operation = Imm16(immediateValue)

      object Far {
        private def Static =
          new Static(0xCB.toByte :: Nil, "retf") with NoDisplacement with NoImmediate

        protected def Imm16(immediateValue: ImmediateValue[?] & WordSize): X86Operation =
          new Static(0xCA.toByte :: Nil, "retf") with NoDisplacement with Immediate[WordSize](immediateValue.withSizePrefixRequirement(noOperandSizePrefixRequirement), source)

        def apply(): X86Operation = Static

        def apply(immediateValue: ImmediateValue[?] & WordSize): X86Operation = Imm16(immediateValue)
      }

    }

  }

  trait LegacyOperations extends ReturnOperations with CallOperations {
    self: ProcessorMode.LegacyBounds & ProcessorMode & OperandSizeInfo =>

    object Call extends BaseCall {

      def apply(nearPointer: NearPointer & WordSize): X86Operation =
        Rel16(nearPointer)

      def apply[Size <: WordDoubleSize](operand: ModRMEncodableOperand & Size): X86Operation =
        RM16(operand)

      object Far {
        def apply(farPointer: FarPointer[WordSize] & FarPointerSize[WordSize]): Static & FarPointerOperation[WordSize] =
          Ptr1616(farPointer)

        def apply(pointer: MemoryLocation & WordSize): X86Operation =
          M1616(pointer)
      }

      def apply(targetLabel: Label): RelativeReference = {
        LabelJumpOperation(
          Seq(new JumpOption(3, Short.MinValue, Short.MaxValue) {
            override def encodableForPointer(offset: Int): Resource & UnlabeledEncodable =
              Rel16(LongPointer.realMode(offset))
          }),
          mnemonic,
          targetLabel
        )
      }
    }
  }

  trait RealOperations extends ReturnOperations with CallOperations {
    self: ProcessorMode.I386Bounds & ProcessorMode & OperandSizeInfo =>

    object Call extends BaseCall {

      def apply(nearPointer: NearPointer & WordDoubleSize): X86Operation =
        nearPointer match {
          case p: WordSize =>
            Rel16(p)
          case p: DoubleWordSize =>
            Rel32(p)
        }

      def apply[Size <: WordDoubleSize](operand: ModRMEncodableOperand & Size): X86Operation =
        RM16(operand)

      object Far {
        def apply[Size <: WordDoubleSize](farPointer: FarPointer[Size] & FarPointerSize[Size]): Static & FarPointerOperation[Size] =
          Ptr1616(farPointer)

        def apply(pointer: MemoryLocation & WordDoubleSize): X86Operation =
          M1616(pointer)
      }

      def apply(targetLabel: Label): RelativeReference = {
        LabelJumpOperation(
          Seq(
            new JumpOption(3, Short.MinValue, Short.MaxValue) {
              override def encodableForPointer(offset: Int): Resource & UnlabeledEncodable =
                Rel16(LongPointer.realMode(offset))
            },
            new JumpOption(6, Int.MinValue, Int.MaxValue) {
              override def encodableForPointer(offset: Int): Resource & UnlabeledEncodable =
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
    self: ProcessorMode.I386Bounds & ProcessorMode & OperandSizeInfo =>

    object Call extends BaseCall {

      def apply(nearPointer: NearPointer & WordDoubleSize): X86Operation =
        nearPointer match {
          case p: WordSize =>
            Rel16(p)
          case p: DoubleWordSize =>
            Rel32(p)
        }

      def apply[Size <: WordDoubleSize](operand: ModRMEncodableOperand & Size): X86Operation =
        RM16(operand)

      object Far {
        def apply[Size <: WordDoubleSize](farPointer: FarPointer[Size] & FarPointerSize[Size]): Static & FarPointerOperation[Size] =
          Ptr1616(farPointer)

        def apply(pointer: MemoryLocation & WordDoubleSize): X86Operation =
          M1616(pointer)
      }

      def apply(targetLabel: Label): RelativeReference = {
        LabelJumpOperation(
          Seq(
            new JumpOption(4, Short.MinValue, Short.MaxValue) {
              override def encodableForPointer(offset: Int): Resource & UnlabeledEncodable =
                Rel16(LongPointer.realMode(offset))
            },
            new JumpOption(5, Int.MinValue, Int.MaxValue) {
              override def encodableForPointer(offset: Int): Resource & UnlabeledEncodable =
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
    self: ProcessorMode.LongBounds & ProcessorMode & OperandSizeInfo =>

    object Call extends BaseCall {

      def apply(nearPointer: NearPointer & DoubleWordSize): X86Operation =
        Rel32(nearPointer)

      def apply(operand: ModRMEncodableOperand & QuadWordSize): X86Operation =
        RM16(operand)

      object Far {
        def apply[Size <: MaxWideSize](pointer: MemoryLocation & Size): X86Operation =
          M1616(pointer)
      }

      def apply(targetLabel: Label): RelativeReference = {
        LabelJumpOperation(
          Seq(
            new JumpOption(5, Int.MinValue, Int.MaxValue) {
              override def encodableForPointer(offset: Int): Resource & UnlabeledEncodable =
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
