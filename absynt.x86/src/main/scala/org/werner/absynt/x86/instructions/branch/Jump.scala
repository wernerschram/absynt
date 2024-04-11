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
import org.werner.absynt.x86.operations.{ModRM, NoDisplacement, NoImmediate, OperandSizeInfo, OperandWithOperandSizePrefixInfo, Static, X86Operation, FarPointer => FarPointerOperation, NearPointer => NearPointerOperation}
import scala.language.implicitConversions

object Jump {
  trait Common {
    self: ArchitectureBounds & ProcessorMode & OperandSizeInfo =>

    sealed abstract class Jump(val shortOpcode: Seq[Byte], val mnemonic: String) {

      protected def Rel8(nearPointer: NearPointer & ByteSize): Static & NearPointerOperation[ByteSize] & NoImmediate =
        new Static(shortOpcode, mnemonic)
          with NearPointerOperation[ByteSize](nearPointer, destination)
          with NoImmediate

      protected def RM16[Size <: WordDoubleQuadSize](operand: ModRMEncodableOperand & Size): ModRM[ModRMEncodableOperand & Size] & NoDisplacement & NoImmediate =
        new ModRM(operand, 0xff.toByte :: Nil, 4, mnemonic, destination, false)
          with NoDisplacement
          with NoImmediate

      protected def Ptr1616[Size <: WordDoubleSize](farPointer: FarPointer[Size] & FarPointerSize[Size]): Static & FarPointerOperation[Size] & NoImmediate =
        new Static(0xEA.toByte :: Nil, mnemonic)
          with FarPointerOperation[Size](farPointer)
          with NoImmediate

      protected def M1616(operand: MemoryLocation & WordDoubleQuadSize): ModRM[MemoryLocation & WordDoubleQuadSize] & NoDisplacement & NoImmediate =
        new ModRM(operand, 0xFF.toByte :: Nil, 5, s"$mnemonic FAR", destination)
          with NoDisplacement
          with NoImmediate
    }

    sealed abstract class ShortRelativeJump(shortOpcode: Seq[Byte], mnemonic: String)
      extends Jump(shortOpcode, mnemonic) {

      def apply(targetLabel: Label): RelativeReference = {
        LabelJumpOperation(
          Seq(new JumpOption(2, Byte.MinValue, Byte.MaxValue) {
            override def encodableForPointer(offset: Int): Resource & UnlabeledEncodable =
              Rel8(ShortPointer(offset.toByte))
          }),
          mnemonic,
          targetLabel
        )
      }

      def apply(nearPointer: NearPointer & ByteSize): X86Operation =
        Rel8(nearPointer)
    }


    sealed abstract class ShortOrLongRelativeJumpI386(shortOpcode: Seq[Byte], val longOpcode: Seq[Byte], mnemonic: String)
      extends Jump(shortOpcode, mnemonic) {

      protected def Rel16(nearPointer: NearPointer & WordSize): Static & NearPointerOperation[WordSize] & NoImmediate =
        new Static(longOpcode, mnemonic)
          with NearPointerOperation[WordSize](nearPointer, destination)
          with NoImmediate

      protected def Rel32(nearPointer: NearPointer & DoubleWordSize): Static & NearPointerOperation[DoubleWordSize] & NoImmediate =
        new Static(longOpcode, mnemonic)
          with NearPointerOperation[DoubleWordSize](nearPointer, destination)
          with NoImmediate
    }
  }

  trait LegacyOperations extends Common {
    self: ProcessorMode.LegacyBounds & ProcessorMode & OperandSizeInfo =>

    sealed abstract class ShortOrLongRelativeJumpLegacy(shortOpcode: Seq[Byte], val longOpcode: Seq[Byte], mnemonic: String)
      extends Jump(shortOpcode, mnemonic) {
      def apply(nearPointer: NearPointer & (ByteSize | WordSize)): X86Operation =
        nearPointer match {
          case p: ByteSize =>
            Rel8(p)
          case p: WordSize =>
            Rel16(p)
        }

      private def Rel16(nearPointer: NearPointer & WordSize) =
        new Static(longOpcode, mnemonic)
          with NearPointerOperation[WordSize](nearPointer, destination)
          with NoImmediate

      def apply(targetLabel: Label): LabelJumpOperation = {
        LabelJumpOperation(
          Seq(
            new JumpOption(shortOpcode.length + 1, Byte.MinValue, Byte.MaxValue) {
              override def encodableForPointer(offset: Int): Resource & UnlabeledEncodable =
                Rel8(ShortPointer(offset.toByte))
            },
            new JumpOption(longOpcode.length + 2, Short.MinValue, Short.MaxValue) {
              override def encodableForPointer(offset: Int): Resource & UnlabeledEncodable =
                Rel16(LongPointer.realMode(offset))
            },
          ),
          mnemonic,
          targetLabel
        )
      }
    }

    object Jump extends ShortOrLongRelativeJumpLegacy(0xEB.toByte :: Nil, 0xE9.toByte :: Nil, "jmp") {

      def apply(operand: ModRMEncodableOperand & WordSize): X86Operation =
            RM16(operand)

      object Far {
        def apply(farPointer: FarPointer[WordSize] & FarPointerSize[WordSize]): Static & FarPointerOperation[WordSize] =
          Ptr1616(farPointer)

        def apply(pointer: MemoryLocation & WordSize): X86Operation =
          M1616(pointer)
      }

    }
    object JumpIfAbove extends ShortOrLongRelativeJumpLegacy(Seq(0x77.toByte), Seq(0x0F.toByte, 0x87.toByte), "ja")
    object JumpIfAboveOrEqual extends ShortOrLongRelativeJumpLegacy(Seq(0x73.toByte), Seq(0x0F.toByte, 0x83.toByte), "jae")
    object JumpIfBelow extends ShortOrLongRelativeJumpLegacy(Seq(0x72.toByte), Seq(0x0F.toByte, 0x82.toByte), "jb")
    object JumpIfBelowOrEqual extends ShortOrLongRelativeJumpLegacy(Seq(0x76.toByte), Seq(0x0F.toByte, 0x86.toByte), "jbe")
    object JumpIfCarry extends ShortOrLongRelativeJumpLegacy(Seq(0x72.toByte), Seq(0x0F.toByte, 0x82.toByte), "jc")
    object JumpIfCountZero extends ShortRelativeJump(Seq(0xE3.toByte), "jcx")
    object JumpIfEqual extends ShortOrLongRelativeJumpLegacy(Seq(0x74.toByte), Seq(0x0F.toByte, 0x84.toByte), "je")
    object JumpIfGreater extends ShortOrLongRelativeJumpLegacy(Seq(0x7F.toByte), Seq(0x0F.toByte, 0x8F.toByte), "jg")
    object JumpIfGreaterOrEqual extends ShortOrLongRelativeJumpLegacy(Seq(0x7D.toByte), Seq(0x0F.toByte, 0x8D.toByte), "jge")
    object JumpIfLess extends ShortOrLongRelativeJumpLegacy(Seq(0x7C.toByte), Seq(0x0F.toByte, 0x8C.toByte), "jl")
    object JumpIfLessOrEqual extends ShortOrLongRelativeJumpLegacy(Seq(0x7E.toByte), Seq(0x0F.toByte, 0x8E.toByte), "jle")
    object JumpIfNotAbove extends ShortOrLongRelativeJumpLegacy(Seq(0x76.toByte), Seq(0x0F.toByte, 0x86.toByte), "jna")
    object JumpIfNotAboveOrEqual extends ShortOrLongRelativeJumpLegacy(Seq(0x72.toByte), Seq(0x0F.toByte, 0x82.toByte), "jnae")
    object JumpIfNotBelow extends ShortOrLongRelativeJumpLegacy(Seq(0x73.toByte), Seq(0x0F.toByte, 0x83.toByte), "jnb")
    object JumpIfNotBelowOrEqual extends ShortOrLongRelativeJumpLegacy(Seq(0x77.toByte), Seq(0x0F.toByte, 0x87.toByte), "jnbe")
    object JumpIfNoCarry extends ShortOrLongRelativeJumpLegacy(Seq(0x73.toByte), Seq(0x0F.toByte, 0x83.toByte), "jnc")
    object JumpIfNotEqual extends ShortOrLongRelativeJumpLegacy(Seq(0x75.toByte), Seq(0x0F.toByte, 0x85.toByte), "jne")
    object JumpIfNotGreater extends ShortOrLongRelativeJumpLegacy(Seq(0x7E.toByte), Seq(0x0F.toByte, 0x8E.toByte), "jng")
    object JumpIfNotGreaterOrEqual extends ShortOrLongRelativeJumpLegacy(Seq(0x7C.toByte), Seq(0x0F.toByte, 0x8C.toByte), "jnge")
    object JumpIfNotLess extends ShortOrLongRelativeJumpLegacy(Seq(0x7D.toByte), Seq(0x0F.toByte, 0x8D.toByte), "jnl")
    object JumpIfNotLessOrEqual extends ShortOrLongRelativeJumpLegacy(Seq(0x7F.toByte), Seq(0x0F.toByte, 0x8F.toByte), "jnle")
    object JumpIfNotOverflow extends ShortOrLongRelativeJumpLegacy(Seq(0x71.toByte), Seq(0x0F.toByte, 0x81.toByte), "jno")
    object JumpIfNotParity extends ShortOrLongRelativeJumpLegacy(Seq(0x7B.toByte), Seq(0x0F.toByte, 0x8B.toByte), "jnp")
    object JumpIfNotSigned extends ShortOrLongRelativeJumpLegacy(Seq(0x79.toByte), Seq(0x0F.toByte, 0x8B.toByte), "jns")
    object JumpIfNotZero extends ShortOrLongRelativeJumpLegacy(Seq(0x75.toByte), Seq(0x0F.toByte, 0x85.toByte), "jnz")
    object JumpIfOverflow extends ShortOrLongRelativeJumpLegacy(Seq(0x70.toByte), Seq(0x0F.toByte, 0x80.toByte), "jo")
    object JumpIfParity extends ShortOrLongRelativeJumpLegacy(Seq(0x7A.toByte), Seq(0x0F.toByte, 0x8A.toByte), "jp")
    object JumpIfParityEven extends ShortOrLongRelativeJumpLegacy(Seq(0x7A.toByte), Seq(0x0F.toByte, 0x8A.toByte), "jpe")
    object JumpIfParityOdd extends ShortOrLongRelativeJumpLegacy(Seq(0x7B.toByte), Seq(0x0F.toByte, 0x8B.toByte), "jpo")
    object JumpIfSigned extends ShortOrLongRelativeJumpLegacy(Seq(0x78.toByte), Seq(0x0F.toByte, 0x88.toByte), "js")
    object JumpIfZero extends ShortOrLongRelativeJumpLegacy(Seq(0x74.toByte), Seq(0x0F.toByte, 0x84.toByte), "jz")
  }

  trait RealOperations extends Common {
    self: ProcessorMode.I386Bounds & ProcessorMode & OperandSizeInfo =>

    sealed abstract class ShortOrLongRelativeJumpReal(shortOpcode: Seq[Byte], longOpcode: Seq[Byte], mnemonic: String)
      extends ShortOrLongRelativeJumpI386(shortOpcode, longOpcode, mnemonic){
      def apply(nearPointer: NearPointer & ByteWordDoubleSize): X86Operation =
        nearPointer match {
          case p: ByteSize =>
            Rel8(p)
          case p: WordSize =>
            Rel16(p)
          case p: DoubleWordSize =>
            Rel32(p)
        }

      def apply(targetLabel: Label): LabelJumpOperation = {
        LabelJumpOperation(
          Seq(
            new JumpOption(shortOpcode.length + 1, Byte.MinValue, Byte.MaxValue) {
              override def encodableForPointer(offset: Int): Resource & UnlabeledEncodable =
                Rel8(ShortPointer(offset.toByte))
            },
            new JumpOption(longOpcode.length + 2, Short.MinValue, Short.MaxValue) {
              override def encodableForPointer(offset: Int): Resource & UnlabeledEncodable =
                Rel16(LongPointer.realMode(offset))
            },
            new JumpOption(1 + longOpcode.length + 4, Int.MinValue, Int.MaxValue) {
              override def encodableForPointer(offset: Int): Resource & UnlabeledEncodable =
                Rel32(LongPointer.protectedMode(offset))
            },
          ),
          mnemonic,
          targetLabel
        )
      }
    }

    object Jump extends ShortOrLongRelativeJumpReal(0xEB.toByte :: Nil, 0xE9.toByte :: Nil, "jmp") {

      def apply[Size<:WordDoubleSize](operand: ModRMEncodableOperand & Size): X86Operation =
        RM16(operand)

      object Far {
        def apply[Size<:WordDoubleSize](farPointer: FarPointer[Size] & FarPointerSize[Size]): Static & FarPointerOperation[Size] =
          Ptr1616(farPointer)

        def apply(pointer: MemoryLocation & WordDoubleSize): X86Operation =
          M1616(pointer)
      }

    }
    object JumpIfAbove extends ShortOrLongRelativeJumpReal(Seq(0x77.toByte), Seq(0x0F.toByte, 0x87.toByte), "ja")
    object JumpIfAboveOrEqual extends ShortOrLongRelativeJumpReal(Seq(0x73.toByte), Seq(0x0F.toByte, 0x83.toByte), "jae")
    object JumpIfBelow extends ShortOrLongRelativeJumpReal(Seq(0x72.toByte), Seq(0x0F.toByte, 0x82.toByte), "jb")
    object JumpIfBelowOrEqual extends ShortOrLongRelativeJumpReal(Seq(0x76.toByte), Seq(0x0F.toByte, 0x86.toByte), "jbe")
    object JumpIfCarry extends ShortOrLongRelativeJumpReal(Seq(0x72.toByte), Seq(0x0F.toByte, 0x82.toByte), "jc")
    object JumpIfCountZero extends ShortRelativeJump(Seq(0xE3.toByte), "jcx")
    object JumpIfEqual extends ShortOrLongRelativeJumpReal(Seq(0x74.toByte), Seq(0x0F.toByte, 0x84.toByte), "je")
    object JumpIfGreater extends ShortOrLongRelativeJumpReal(Seq(0x7F.toByte), Seq(0x0F.toByte, 0x8F.toByte), "jg")
    object JumpIfGreaterOrEqual extends ShortOrLongRelativeJumpReal(Seq(0x7D.toByte), Seq(0x0F.toByte, 0x8D.toByte), "jge")
    object JumpIfLess extends ShortOrLongRelativeJumpReal(Seq(0x7C.toByte), Seq(0x0F.toByte, 0x8C.toByte), "jl")
    object JumpIfLessOrEqual extends ShortOrLongRelativeJumpReal(Seq(0x7E.toByte), Seq(0x0F.toByte, 0x8E.toByte), "jle")
    object JumpIfNotAbove extends ShortOrLongRelativeJumpReal(Seq(0x76.toByte), Seq(0x0F.toByte, 0x86.toByte), "jna")
    object JumpIfNotAboveOrEqual extends ShortOrLongRelativeJumpReal(Seq(0x72.toByte), Seq(0x0F.toByte, 0x82.toByte), "jnae")
    object JumpIfNotBelow extends ShortOrLongRelativeJumpReal(Seq(0x73.toByte), Seq(0x0F.toByte, 0x83.toByte), "jnb")
    object JumpIfNotBelowOrEqual extends ShortOrLongRelativeJumpReal(Seq(0x77.toByte), Seq(0x0F.toByte, 0x87.toByte), "jnbe")
    object JumpIfNoCarry extends ShortOrLongRelativeJumpReal(Seq(0x73.toByte), Seq(0x0F.toByte, 0x83.toByte), "jnc")
    object JumpIfNotEqual extends ShortOrLongRelativeJumpReal(Seq(0x75.toByte), Seq(0x0F.toByte, 0x85.toByte), "jne")
    object JumpIfNotGreater extends ShortOrLongRelativeJumpReal(Seq(0x7E.toByte), Seq(0x0F.toByte, 0x8E.toByte), "jng")
    object JumpIfNotGreaterOrEqual extends ShortOrLongRelativeJumpReal(Seq(0x7C.toByte), Seq(0x0F.toByte, 0x8C.toByte), "jnge")
    object JumpIfNotLess extends ShortOrLongRelativeJumpReal(Seq(0x7D.toByte), Seq(0x0F.toByte, 0x8D.toByte), "jnl")
    object JumpIfNotLessOrEqual extends ShortOrLongRelativeJumpReal(Seq(0x7F.toByte), Seq(0x0F.toByte, 0x8F.toByte), "jnle")
    object JumpIfNotOverflow extends ShortOrLongRelativeJumpReal(Seq(0x71.toByte), Seq(0x0F.toByte, 0x81.toByte), "jno")
    object JumpIfNotParity extends ShortOrLongRelativeJumpReal(Seq(0x7B.toByte), Seq(0x0F.toByte, 0x8B.toByte), "jnp")
    object JumpIfNotSigned extends ShortOrLongRelativeJumpReal(Seq(0x79.toByte), Seq(0x0F.toByte, 0x8B.toByte), "jns")
    object JumpIfNotZero extends ShortOrLongRelativeJumpReal(Seq(0x75.toByte), Seq(0x0F.toByte, 0x85.toByte), "jnz")
    object JumpIfOverflow extends ShortOrLongRelativeJumpReal(Seq(0x70.toByte), Seq(0x0F.toByte, 0x80.toByte), "jo")
    object JumpIfParity extends ShortOrLongRelativeJumpReal(Seq(0x7A.toByte), Seq(0x0F.toByte, 0x8A.toByte), "jp")
    object JumpIfParityEven extends ShortOrLongRelativeJumpReal(Seq(0x7A.toByte), Seq(0x0F.toByte, 0x8A.toByte), "jpe")
    object JumpIfParityOdd extends ShortOrLongRelativeJumpReal(Seq(0x7B.toByte), Seq(0x0F.toByte, 0x8B.toByte), "jpo")
    object JumpIfSigned extends ShortOrLongRelativeJumpReal(Seq(0x78.toByte), Seq(0x0F.toByte, 0x88.toByte), "js")
    object JumpIfZero extends ShortOrLongRelativeJumpReal(Seq(0x74.toByte), Seq(0x0F.toByte, 0x84.toByte), "jz")
  }

  trait ProtectedOperations extends Common {
    self: ProcessorMode.I386Bounds & ProcessorMode & OperandSizeInfo =>

    sealed abstract class ShortOrLongRelativeJumpProtected(shortOpcode: Seq[Byte], longOpcode: Seq[Byte], mnemonic: String)
      extends ShortOrLongRelativeJumpI386(shortOpcode, longOpcode, mnemonic){

      def apply(nearPointer: NearPointer & ByteWordDoubleSize): X86Operation =
        nearPointer match {
          case p: ByteSize =>
            Rel8(p)
          case p: WordSize =>
            Rel16(p)
          case p: DoubleWordSize =>
            Rel32(p)
        }

      def apply(targetLabel: Label): LabelJumpOperation = {
        LabelJumpOperation(
          Seq(
            new JumpOption(shortOpcode.length + 1, Byte.MinValue, Byte.MaxValue) {
              override def encodableForPointer(offset: Int): Resource & UnlabeledEncodable =
                Rel8(ShortPointer(offset.toByte))
            },
            new JumpOption(1 + longOpcode.length + 2, Short.MinValue, Short.MaxValue) {
              override def encodableForPointer(offset: Int): Resource & UnlabeledEncodable =
                Rel16(LongPointer.realMode(offset))
            },
            new JumpOption(longOpcode.length + 4, Int.MinValue, Int.MaxValue) {
              override def encodableForPointer(offset: Int): Resource & UnlabeledEncodable =
                Rel32(LongPointer.protectedMode(offset))
            },
          ),
          mnemonic,
          targetLabel
        )
      }
    }

    object Jump extends ShortOrLongRelativeJumpProtected(0xEB.toByte :: Nil, 0xE9.toByte :: Nil, "jmp") {

      def apply[Size<:WordDoubleSize](operand: ModRMEncodableOperand & Size): X86Operation =
        RM16(operand)

      object Far {
        def apply[Size<:WordDoubleSize](farPointer: FarPointer[Size] & FarPointerSize[Size]): Static & FarPointerOperation[Size] =
          Ptr1616(farPointer)

        def apply(pointer: MemoryLocation & WordDoubleSize): X86Operation =
          M1616(pointer)
      }

    }
    object JumpIfAbove extends ShortOrLongRelativeJumpProtected(Seq(0x77.toByte), Seq(0x0F.toByte, 0x87.toByte), "ja")
    object JumpIfAboveOrEqual extends ShortOrLongRelativeJumpProtected(Seq(0x73.toByte), Seq(0x0F.toByte, 0x83.toByte), "jae")
    object JumpIfBelow extends ShortOrLongRelativeJumpProtected(Seq(0x72.toByte), Seq(0x0F.toByte, 0x82.toByte), "jb")
    object JumpIfBelowOrEqual extends ShortOrLongRelativeJumpProtected(Seq(0x76.toByte), Seq(0x0F.toByte, 0x86.toByte), "jbe")
    object JumpIfCarry extends ShortOrLongRelativeJumpProtected(Seq(0x72.toByte), Seq(0x0F.toByte, 0x82.toByte), "jc")
    object JumpIfCountZero extends ShortRelativeJump(Seq(0xE3.toByte), "jcx")
    object JumpIfEqual extends ShortOrLongRelativeJumpProtected(Seq(0x74.toByte), Seq(0x0F.toByte, 0x84.toByte), "je")
    object JumpIfGreater extends ShortOrLongRelativeJumpProtected(Seq(0x7F.toByte), Seq(0x0F.toByte, 0x8F.toByte), "jg")
    object JumpIfGreaterOrEqual extends ShortOrLongRelativeJumpProtected(Seq(0x7D.toByte), Seq(0x0F.toByte, 0x8D.toByte), "jge")
    object JumpIfLess extends ShortOrLongRelativeJumpProtected(Seq(0x7C.toByte), Seq(0x0F.toByte, 0x8C.toByte), "jl")
    object JumpIfLessOrEqual extends ShortOrLongRelativeJumpProtected(Seq(0x7E.toByte), Seq(0x0F.toByte, 0x8E.toByte), "jle")
    object JumpIfNotAbove extends ShortOrLongRelativeJumpProtected(Seq(0x76.toByte), Seq(0x0F.toByte, 0x86.toByte), "jna")
    object JumpIfNotAboveOrEqual extends ShortOrLongRelativeJumpProtected(Seq(0x72.toByte), Seq(0x0F.toByte, 0x82.toByte), "jnae")
    object JumpIfNotBelow extends ShortOrLongRelativeJumpProtected(Seq(0x73.toByte), Seq(0x0F.toByte, 0x83.toByte), "jnb")
    object JumpIfNotBelowOrEqual extends ShortOrLongRelativeJumpProtected(Seq(0x77.toByte), Seq(0x0F.toByte, 0x87.toByte), "jnbe")
    object JumpIfNoCarry extends ShortOrLongRelativeJumpProtected(Seq(0x73.toByte), Seq(0x0F.toByte, 0x83.toByte), "jnc")
    object JumpIfNotEqual extends ShortOrLongRelativeJumpProtected(Seq(0x75.toByte), Seq(0x0F.toByte, 0x85.toByte), "jne")
    object JumpIfNotGreater extends ShortOrLongRelativeJumpProtected(Seq(0x7E.toByte), Seq(0x0F.toByte, 0x8E.toByte), "jng")
    object JumpIfNotGreaterOrEqual extends ShortOrLongRelativeJumpProtected(Seq(0x7C.toByte), Seq(0x0F.toByte, 0x8C.toByte), "jnge")
    object JumpIfNotLess extends ShortOrLongRelativeJumpProtected(Seq(0x7D.toByte), Seq(0x0F.toByte, 0x8D.toByte), "jnl")
    object JumpIfNotLessOrEqual extends ShortOrLongRelativeJumpProtected(Seq(0x7F.toByte), Seq(0x0F.toByte, 0x8F.toByte), "jnle")
    object JumpIfNotOverflow extends ShortOrLongRelativeJumpProtected(Seq(0x71.toByte), Seq(0x0F.toByte, 0x81.toByte), "jno")
    object JumpIfNotParity extends ShortOrLongRelativeJumpProtected(Seq(0x7B.toByte), Seq(0x0F.toByte, 0x8B.toByte), "jnp")
    object JumpIfNotSigned extends ShortOrLongRelativeJumpProtected(Seq(0x79.toByte), Seq(0x0F.toByte, 0x8B.toByte), "jns")
    object JumpIfNotZero extends ShortOrLongRelativeJumpProtected(Seq(0x75.toByte), Seq(0x0F.toByte, 0x85.toByte), "jnz")
    object JumpIfOverflow extends ShortOrLongRelativeJumpProtected(Seq(0x70.toByte), Seq(0x0F.toByte, 0x80.toByte), "jo")
    object JumpIfParity extends ShortOrLongRelativeJumpProtected(Seq(0x7A.toByte), Seq(0x0F.toByte, 0x8A.toByte), "jp")
    object JumpIfParityEven extends ShortOrLongRelativeJumpProtected(Seq(0x7A.toByte), Seq(0x0F.toByte, 0x8A.toByte), "jpe")
    object JumpIfParityOdd extends ShortOrLongRelativeJumpProtected(Seq(0x7B.toByte), Seq(0x0F.toByte, 0x8B.toByte), "jpo")
    object JumpIfSigned extends ShortOrLongRelativeJumpProtected(Seq(0x78.toByte), Seq(0x0F.toByte, 0x88.toByte), "js")
    object JumpIfZero extends ShortOrLongRelativeJumpProtected(Seq(0x74.toByte), Seq(0x0F.toByte, 0x84.toByte), "jz")
  }

  trait LongOperations extends Common {
    self: ProcessorMode.LongBounds & ProcessorMode & OperandSizeInfo =>

    abstract class ShortOrLongRelativeJumpLong(shortOpcode: Seq[Byte], longOpcode: Seq[Byte], mnemonic: String)
      extends ShortOrLongRelativeJumpI386(shortOpcode, longOpcode, mnemonic) {
      def apply(nearPointer: NearPointer & (ByteSize | DoubleWordSize)): X86Operation =
        nearPointer match {
          case p: ByteSize =>
            Rel8(p)
          case p: DoubleWordSize =>
            Rel32(p)
        }

      def apply(targetLabel: Label): LabelJumpOperation = {
        LabelJumpOperation(
          Seq(
            new JumpOption(shortOpcode.length + 1, Byte.MinValue, Byte.MaxValue) {
              override def encodableForPointer(offset: Int): Resource & UnlabeledEncodable =
                Rel8(ShortPointer(offset.toByte))
            },
            new JumpOption(longOpcode.length + 4, Int.MinValue, Int.MaxValue) {
              override def encodableForPointer(offset: Int): Resource & UnlabeledEncodable =
                Rel32(LongPointer.protectedMode(offset))
            }
          ),
          mnemonic,
          targetLabel
        )
      }
    }

    object Jump extends ShortOrLongRelativeJumpLong(0xEB.toByte :: Nil, 0xE9.toByte :: Nil, "jmp") {

      def apply(operand: ModRMEncodableOperand & QuadWordSize): X86Operation =
        RM16(operand)

      def rm(operand: ModRMEncodableOperand & QuadWordSize): X86Operation =
        RM16(operand)

      object Far {
        def apply(pointer: MemoryLocation & WordDoubleQuadSize): X86Operation =
          M1616(pointer)
      }
    }
    object JumpIfAbove extends ShortOrLongRelativeJumpLong(Seq(0x77.toByte), Seq(0x0F.toByte, 0x87.toByte), "ja")
    object JumpIfAboveOrEqual extends ShortOrLongRelativeJumpLong(Seq(0x73.toByte), Seq(0x0F.toByte, 0x83.toByte), "jae")
    object JumpIfBelow extends ShortOrLongRelativeJumpLong(Seq(0x72.toByte), Seq(0x0F.toByte, 0x82.toByte), "jb")
    object JumpIfBelowOrEqual extends ShortOrLongRelativeJumpLong(Seq(0x76.toByte), Seq(0x0F.toByte, 0x86.toByte), "jbe")
    object JumpIfCarry extends ShortOrLongRelativeJumpLong(Seq(0x72.toByte), Seq(0x0F.toByte, 0x82.toByte), "jc")
    object JumpIfCountZero extends ShortRelativeJump(Seq(0xE3.toByte), "jcx")
    object JumpIfEqual extends ShortOrLongRelativeJumpLong(Seq(0x74.toByte), Seq(0x0F.toByte, 0x84.toByte), "je")
    object JumpIfGreater extends ShortOrLongRelativeJumpLong(Seq(0x7F.toByte), Seq(0x0F.toByte, 0x8F.toByte), "jg")
    object JumpIfGreaterOrEqual extends ShortOrLongRelativeJumpLong(Seq(0x7D.toByte), Seq(0x0F.toByte, 0x8D.toByte), "jge")
    object JumpIfLess extends ShortOrLongRelativeJumpLong(Seq(0x7C.toByte), Seq(0x0F.toByte, 0x8C.toByte), "jl")
    object JumpIfLessOrEqual extends ShortOrLongRelativeJumpLong(Seq(0x7E.toByte), Seq(0x0F.toByte, 0x8E.toByte), "jle")
    object JumpIfNotAbove extends ShortOrLongRelativeJumpLong(Seq(0x76.toByte), Seq(0x0F.toByte, 0x86.toByte), "jna")
    object JumpIfNotAboveOrEqual extends ShortOrLongRelativeJumpLong(Seq(0x72.toByte), Seq(0x0F.toByte, 0x82.toByte), "jnae")
    object JumpIfNotBelow extends ShortOrLongRelativeJumpLong(Seq(0x73.toByte), Seq(0x0F.toByte, 0x83.toByte), "jnb")
    object JumpIfNotBelowOrEqual extends ShortOrLongRelativeJumpLong(Seq(0x77.toByte), Seq(0x0F.toByte, 0x87.toByte), "jnbe")
    object JumpIfNoCarry extends ShortOrLongRelativeJumpLong(Seq(0x73.toByte), Seq(0x0F.toByte, 0x83.toByte), "jnc")
    object JumpIfNotEqual extends ShortOrLongRelativeJumpLong(Seq(0x75.toByte), Seq(0x0F.toByte, 0x85.toByte), "jne")
    object JumpIfNotGreater extends ShortOrLongRelativeJumpLong(Seq(0x7E.toByte), Seq(0x0F.toByte, 0x8E.toByte), "jng")
    object JumpIfNotGreaterOrEqual extends ShortOrLongRelativeJumpLong(Seq(0x7C.toByte), Seq(0x0F.toByte, 0x8C.toByte), "jnge")
    object JumpIfNotLess extends ShortOrLongRelativeJumpLong(Seq(0x7D.toByte), Seq(0x0F.toByte, 0x8D.toByte), "jnl")
    object JumpIfNotLessOrEqual extends ShortOrLongRelativeJumpLong(Seq(0x7F.toByte), Seq(0x0F.toByte, 0x8F.toByte), "jnle")
    object JumpIfNotOverflow extends ShortOrLongRelativeJumpLong(Seq(0x71.toByte), Seq(0x0F.toByte, 0x81.toByte), "jno")
    object JumpIfNotParity extends ShortOrLongRelativeJumpLong(Seq(0x7B.toByte), Seq(0x0F.toByte, 0x8B.toByte), "jnp")
    object JumpIfNotSigned extends ShortOrLongRelativeJumpLong(Seq(0x79.toByte), Seq(0x0F.toByte, 0x8B.toByte), "jns")
    object JumpIfNotZero extends ShortOrLongRelativeJumpLong(Seq(0x75.toByte), Seq(0x0F.toByte, 0x85.toByte), "jnz")
    object JumpIfOverflow extends ShortOrLongRelativeJumpLong(Seq(0x70.toByte), Seq(0x0F.toByte, 0x80.toByte), "jo")
    object JumpIfParity extends ShortOrLongRelativeJumpLong(Seq(0x7A.toByte), Seq(0x0F.toByte, 0x8A.toByte), "jp")
    object JumpIfParityEven extends ShortOrLongRelativeJumpLong(Seq(0x7A.toByte), Seq(0x0F.toByte, 0x8A.toByte), "jpe")
    object JumpIfParityOdd extends ShortOrLongRelativeJumpLong(Seq(0x7B.toByte), Seq(0x0F.toByte, 0x8B.toByte), "jpo")
    object JumpIfSigned extends ShortOrLongRelativeJumpLong(Seq(0x78.toByte), Seq(0x0F.toByte, 0x88.toByte), "js")
    object JumpIfZero extends ShortOrLongRelativeJumpLong(Seq(0x74.toByte), Seq(0x0F.toByte, 0x84.toByte), "jz")
  }
}
