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

import org.werner.absynt.Label
import org.werner.absynt.resource.{RelativeReference, Resource, UnlabeledEncodable}
import org.werner.absynt.x86.HasOperandSizePrefixRequirements
import org.werner.absynt.x86.operands.memoryaccess._
import org.werner.absynt.x86.operands._
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder._
import org.werner.absynt.x86.operations.branch.{LongJumpOperation, NearJumpOperation, ShortJumpOperation}
import org.werner.absynt.x86.operations.{Immediate, ModRM, NoDisplacement, NoImmediate, OperandSizePrefixRequirement, Static, X86Operation, FarPointer => FarPointerOperation, NearPointer => NearPointerOperation}

sealed abstract class Jump(val shortOpcode: Seq[Byte], implicit val mnemonic: String) {
  self: HasOperandSizePrefixRequirements =>

  protected def Rel8(nearPointer: NearPointer with ByteSize): Static with NearPointerOperation[ByteSize] with NoImmediate =
    new Static(shortOpcode, mnemonic) with NearPointerOperation[ByteSize] with NoImmediate with HasOperandSizePrefixRequirements {
      override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = Jump.this.operandSizePrefixRequirement

      override val pointer: NearPointer with ByteSize = nearPointer

      override def pointerOrder: OperandOrder = destination
    }

  protected def RM16[Size <: WordDoubleQuadSize](operand: ModRMEncodableOperand with Size) =
    new ModRM(operand, 0xff.toByte :: Nil, 4, mnemonic, destination, false) with NoDisplacement with NoImmediate

  protected def Ptr1616[Size <: WordDoubleSize](farPointer: FarPointer[Size] with FarPointerSize[Size]): Static with FarPointerOperation[Size] with NoImmediate with HasOperandSizePrefixRequirements =
    new Static(0xEA.toByte :: Nil, mnemonic) with FarPointerOperation[Size] with NoImmediate with HasOperandSizePrefixRequirements {
      override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = Jump.this.operandSizePrefixRequirement

      override def pointer: FarPointer[Size] with FarPointerSize[Size] = farPointer
    }

  protected def M1616(operand: MemoryLocation with WordDoubleQuadSize) =
    new ModRM(operand, 0xFF.toByte :: Nil, 5, s"$mnemonic FAR", destination) with NoDisplacement with NoImmediate
}

sealed abstract class ShortRelativeJump(shortOpcode: Seq[Byte], mnemonic: String)
  extends Jump(shortOpcode, mnemonic) {
  self: HasOperandSizePrefixRequirements =>

  def apply(targetLabel: Label): RelativeReference = {
    new ShortJumpOperation(shortOpcode, mnemonic, targetLabel) {
      override def encodableForShortPointer(offset: Byte): Resource with UnlabeledEncodable =
        Rel8(ShortPointer(offset))
    }
  }

  def apply(nearPointer: NearPointer with ByteSize): X86Operation =
    Rel8(nearPointer)
}


sealed abstract class ShortOrLongRelativeJumpI386(shortOpcode: Seq[Byte], val longOpcode: Seq[Byte], mnemonic: String)
  extends Jump(shortOpcode, mnemonic) {
  self: HasOperandSizePrefixRequirements =>

  protected def Rel16(nearPointer: NearPointer with WordSize): Static with NearPointerOperation[WordSize] with NoImmediate = {
    new Static(longOpcode, mnemonic) with NearPointerOperation[WordSize] with NoImmediate with HasOperandSizePrefixRequirements {
      override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = ShortOrLongRelativeJumpI386.this.operandSizePrefixRequirement
      override val pointer: NearPointer with WordSize = nearPointer

      override def pointerOrder: OperandOrder = destination
    }
  }

  protected def Rel32(nearPointer: NearPointer with DoubleWordSize): Static with NearPointerOperation[DoubleWordSize] with NoImmediate = {
    new Static(longOpcode, mnemonic) with NearPointerOperation[DoubleWordSize] with NoImmediate with HasOperandSizePrefixRequirements {
      override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = ShortOrLongRelativeJumpI386.this.operandSizePrefixRequirement

      override val pointer: NearPointer with DoubleWordSize = nearPointer

      override def pointerOrder: OperandOrder = destination
    }
  }
}

sealed trait CallOperations {
  self: HasOperandSizePrefixRequirements =>

  val mnemonic = "call"
  val opcode = Seq(0xe8.toByte)

  protected def Rel16(nearPointer: NearPointer with WordSize): Static with NearPointerOperation[WordSize] with NoImmediate = {
    new Static(opcode, mnemonic) with NearPointerOperation[WordSize] with NoImmediate with HasOperandSizePrefixRequirements {
      override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = CallOperations.this.operandSizePrefixRequirement

      override val pointer: NearPointer with WordSize = nearPointer

      override def pointerOrder: OperandOrder = destination
    }
  }

  protected def Rel32(nearPointer: NearPointer with DoubleWordSize): Static with NearPointerOperation[DoubleWordSize] with NoImmediate = {
    new Static(opcode, mnemonic) with NearPointerOperation[DoubleWordSize] with NoImmediate with HasOperandSizePrefixRequirements {
      override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = CallOperations.this.operandSizePrefixRequirement

      override val pointer: NearPointer with DoubleWordSize = nearPointer

      override def pointerOrder: OperandOrder = destination
    }
  }

  protected def RM16[Size<:WordDoubleQuadSize](operand: ModRMEncodableOperand with Size) =
    new ModRM(operand, 0xff.toByte :: Nil, 2, mnemonic, destination, false) with NoDisplacement with NoImmediate

  protected def Ptr1616[Size<:WordDoubleSize](farPointer: FarPointer[Size] with FarPointerSize[Size]): Static with FarPointerOperation[Size] with NoImmediate with HasOperandSizePrefixRequirements =
    new Static(0x9A.toByte :: Nil, mnemonic) with FarPointerOperation[Size] with NoImmediate with HasOperandSizePrefixRequirements {
      override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = CallOperations.this.operandSizePrefixRequirement
      override def pointer: FarPointer[Size] with FarPointerSize[Size] = farPointer
    }

  protected def M1616(operand: MemoryLocation with WordDoubleQuadSize) =
    new ModRM(operand, 0xFF.toByte :: Nil, 3, s"$mnemonic FAR", destination) with NoDisplacement with NoImmediate
}

object Jump {
  trait ReturnOperations {
    self: HasOperandSizePrefixRequirements =>
    object Return {

      val mnemonic: String = "ret"

      private def Static =
        new Static(0xC3.toByte :: Nil, "ret") with NoDisplacement with NoImmediate

      protected def Imm16(immediateValue: ImmediateValue with WordSize): X86Operation =
        new Static(0xC2.toByte :: Nil, "ret") with NoDisplacement with Immediate[WordSize] with HasOperandSizePrefixRequirements {
          override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = new OperandSizePrefixRequirement {
            override def normalOperand(size: Operand with ValueSize): Boolean = false
            override def pointerOperand(size: Operand with FarPointerSize[_]): Boolean = false
          }

          override def immediate: ImmediateValue with WordSize = immediateValue

          override def immediateOrder: OperandOrder = source
        }


      def apply(): X86Operation = Static

      def apply(immediateValue: ImmediateValue with WordSize): X86Operation = Imm16(immediateValue)

      object Far {
        private def Static =
          new Static(0xCB.toByte :: Nil, "retf") with NoDisplacement with NoImmediate

        protected def Imm16(immediateValue: ImmediateValue with WordSize): X86Operation =
          new Static(0xCA.toByte :: Nil, "retf") with NoDisplacement with Immediate[WordSize] with HasOperandSizePrefixRequirements {
            override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = new OperandSizePrefixRequirement {
            override def normalOperand(size: Operand with ValueSize): Boolean = false
            override def pointerOperand(size: Operand with FarPointerSize[_]): Boolean = false
          }

            override def immediate: ImmediateValue with WordSize = immediateValue

            override def immediateOrder: OperandOrder = source
          }

        def apply(): X86Operation = Static

        def apply(immediateValue: ImmediateValue with WordSize): X86Operation = Imm16(immediateValue)
      }
    }
  }

  trait LegacyOperations extends ReturnOperations {
    self: HasOperandSizePrefixRequirements =>

    sealed abstract class ShortOrLongRelativeJumpLegacy(shortOpcode: Seq[Byte], val longOpcode: Seq[Byte], mnemonic: String)
      extends Jump(shortOpcode, mnemonic) with HasOperandSizePrefixRequirements {

      override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = LegacyOperations.this.operandSizePrefixRequirement

      // TODO: ValueSize is too wide
      def apply(nearPointer: NearPointer with ByteWordSize): X86Operation =
        nearPointer match {
          case p: NearPointer with ByteSize =>
            Rel8(p)
          case p: NearPointer with WordSize =>
            Rel16(p)
        }

      private def Rel16(nearPointer: NearPointer with WordSize) = {
        new Static(longOpcode, mnemonic) with NearPointerOperation[WordSize] with NoImmediate with HasOperandSizePrefixRequirements {
          override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = ShortOrLongRelativeJumpLegacy.this.operandSizePrefixRequirement

          override val pointer: NearPointer with WordSize = nearPointer

          override def pointerOrder: OperandOrder = destination

        }
      }

      def apply(targetLabel: Label): NearJumpOperation[WordSize] = {
        new NearJumpOperation[WordSize](shortOpcode, longOpcode, mnemonic, targetLabel, 2) {
          override def encodableForShortPointer(offset: Byte): Resource with UnlabeledEncodable =
            Rel8(ShortPointer(offset))

          override def encodableForLongPointer(offset: Int): Resource with UnlabeledEncodable =
            Rel16(LongPointer.realMode(offset))
        }
      }
    }

    object Call extends CallOperations with HasOperandSizePrefixRequirements {
      override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = LegacyOperations.this.operandSizePrefixRequirement

      def apply(nearPointer: NearPointer with WordSize): X86Operation =
        Rel16(nearPointer)

      def apply[Size<:WordDoubleSize](operand: ModRMEncodableOperand with Size): X86Operation =
        RM16(operand)

      object Far {
        def apply(farPointer: FarPointer[WordSize] with FarPointerSize[WordSize]): Static with FarPointerOperation[WordSize] =
          Ptr1616(farPointer)

        def apply(pointer: MemoryLocation with WordSize): X86Operation =
          M1616(pointer)
      }

      def apply(targetLabel: Label): LongJumpOperation = {
        new LongJumpOperation(opcode, mnemonic, targetLabel, 4) {
          override def encodableForLongPointer(offset: Int): Resource with UnlabeledEncodable =
            Rel16(LongPointer.realMode(offset))
        }
      }
    }

    object Jump extends ShortOrLongRelativeJumpLegacy(0xEB.toByte :: Nil, 0xE9.toByte :: Nil, "jmp") {

      def apply(operand: ModRMEncodableOperand with WordSize): X86Operation =
            RM16(operand)

      object Far {
        def apply(farPointer: FarPointer[WordSize] with FarPointerSize[WordSize]): Static with FarPointerOperation[WordSize] =
          Ptr1616(farPointer)

        def apply(pointer: MemoryLocation with WordSize): X86Operation =
          M1616(pointer)
      }

    }
    object JumpIfAbove extends ShortOrLongRelativeJumpLegacy(Seq(0x77.toByte), Seq(0x0F.toByte, 0x87.toByte), "ja")
    object JumpIfAboveOrEqual extends ShortOrLongRelativeJumpLegacy(Seq(0x73.toByte), Seq(0x0F.toByte, 0x83.toByte), "jae")
    object JumpIfBelow extends ShortOrLongRelativeJumpLegacy(Seq(0x72.toByte), Seq(0x0F.toByte, 0x82.toByte), "jb")
    object JumpIfBelowOrEqual extends ShortOrLongRelativeJumpLegacy(Seq(0x76.toByte), Seq(0x0F.toByte, 0x86.toByte), "jbe")
    object JumpIfCarry extends ShortOrLongRelativeJumpLegacy(Seq(0x72.toByte), Seq(0x0F.toByte, 0x82.toByte), "jc")
    object JumpIfCountZero extends ShortRelativeJump(Seq(0xE3.toByte), "jcx") with HasOperandSizePrefixRequirements {
      override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = Jump.operandSizePrefixRequirement
    }
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

  trait RealOperations extends ReturnOperations {
    self: HasOperandSizePrefixRequirements =>

    sealed abstract class ShortOrLongRelativeJumpReal(shortOpcode: Seq[Byte], longOpcode: Seq[Byte], mnemonic: String)
      extends ShortOrLongRelativeJumpI386(shortOpcode, longOpcode, mnemonic) with HasOperandSizePrefixRequirements {

      override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = RealOperations.this.operandSizePrefixRequirement

      def apply(nearPointer: NearPointer with ByteWordDoubleSize): X86Operation =
        nearPointer match {
          case p: NearPointer with ByteSize =>
            Rel8(p)
          case p: NearPointer with WordSize =>
            Rel16(p)
          case p: NearPointer with DoubleWordSize =>
            //TODO: FIX! and add test
            Rel32(p)
        }

      // TODO: this ignores the rel16 option
      def apply(targetLabel: Label): NearJumpOperation[WordSize] =
        new NearJumpOperation[WordSize](shortOpcode, longOpcode, mnemonic, targetLabel, 2) {
          override def encodableForShortPointer(offset: Byte): Resource with UnlabeledEncodable =
            Rel8(ShortPointer(offset))

          override def encodableForLongPointer(offset: Int): Resource with UnlabeledEncodable =
            Rel16(LongPointer.realMode(offset))
        }
    }

    object Call extends CallOperations with HasOperandSizePrefixRequirements {
      override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = RealOperations.this.operandSizePrefixRequirement

      def apply(nearPointer: NearPointer with WordDoubleSize): X86Operation =
        nearPointer match {
          case p: NearPointer with WordSize =>
            Rel16(p)
          case p: NearPointer with DoubleWordSize =>
            Rel32(p)
        }

      def apply[Size<:WordDoubleSize](operand: ModRMEncodableOperand with Size): X86Operation =
        RM16(operand)

      object Far {
        def apply[Size<:WordDoubleSize](farPointer: FarPointer[Size] with FarPointerSize[Size]): Static with FarPointerOperation[Size] =
          Ptr1616(farPointer)

        def apply(pointer: MemoryLocation with WordDoubleSize): X86Operation =
          M1616(pointer)
      }

      // TODO: this ignores the rel16 option
      def apply(targetLabel: Label): LongJumpOperation = {
        new LongJumpOperation(opcode, mnemonic, targetLabel, 4) {
          override def encodableForLongPointer(offset: Int): Resource with UnlabeledEncodable =
            Rel32(LongPointer.protectedMode(offset))
        }
      }
    }

    object Jump extends ShortOrLongRelativeJumpReal(0xEB.toByte :: Nil, 0xE9.toByte :: Nil, "jmp") {

      def apply[Size<:WordDoubleSize](operand: ModRMEncodableOperand with Size): X86Operation =
        RM16(operand)

      object Far {
        def apply[Size<:WordDoubleSize](farPointer: FarPointer[Size] with FarPointerSize[Size]): Static with FarPointerOperation[Size] =
          Ptr1616(farPointer)

        def apply(pointer: MemoryLocation with WordDoubleSize): X86Operation =
          M1616(pointer)
      }

    }
    object JumpIfAbove extends ShortOrLongRelativeJumpReal(Seq(0x77.toByte), Seq(0x0F.toByte, 0x87.toByte), "ja")
    object JumpIfAboveOrEqual extends ShortOrLongRelativeJumpReal(Seq(0x73.toByte), Seq(0x0F.toByte, 0x83.toByte), "jae")
    object JumpIfBelow extends ShortOrLongRelativeJumpReal(Seq(0x72.toByte), Seq(0x0F.toByte, 0x82.toByte), "jb")
    object JumpIfBelowOrEqual extends ShortOrLongRelativeJumpReal(Seq(0x76.toByte), Seq(0x0F.toByte, 0x86.toByte), "jbe")
    object JumpIfCarry extends ShortOrLongRelativeJumpReal(Seq(0x72.toByte), Seq(0x0F.toByte, 0x82.toByte), "jc")
    object JumpIfCountZero extends ShortRelativeJump(Seq(0xE3.toByte), "jcx") with HasOperandSizePrefixRequirements {
      override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = Jump.operandSizePrefixRequirement
    }
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

  trait ProtectedOperations extends ReturnOperations {
    self: HasOperandSizePrefixRequirements =>

    sealed abstract class ShortOrLongRelativeJumpProtected(shortOpcode: Seq[Byte], longOpcode: Seq[Byte], mnemonic: String)
      extends ShortOrLongRelativeJumpI386(shortOpcode, longOpcode, mnemonic) with HasOperandSizePrefixRequirements {

      override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = ProtectedOperations.this.operandSizePrefixRequirement

      def apply(nearPointer: NearPointer with ByteWordDoubleSize): X86Operation =
        nearPointer match {
          case p: NearPointer with ByteSize =>
            Rel8(p)
          case p: NearPointer with WordSize =>
            //TODO: FIX! and add test
            Rel16(p)
          case p: NearPointer with DoubleWordSize =>
            Rel32(p)
        }

      // TODO: this ignores the rel16 option
      def apply(targetLabel: Label): NearJumpOperation[DoubleWordSize] =
        new NearJumpOperation[DoubleWordSize](shortOpcode, longOpcode, mnemonic, targetLabel, 4) {
          override def encodableForShortPointer(offset: Byte): Resource with UnlabeledEncodable =
            Rel8(ShortPointer(offset))

          override def encodableForLongPointer(offset: Int): Resource with UnlabeledEncodable =
            Rel32(LongPointer.protectedMode(offset))
        }
    }

    object Call extends CallOperations with HasOperandSizePrefixRequirements {
      override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = ProtectedOperations.this.operandSizePrefixRequirement

      def apply(nearPointer: NearPointer with WordDoubleSize): X86Operation =
        nearPointer match {
          case p: NearPointer with WordSize =>
            Rel16(p)
          case p: NearPointer with DoubleWordSize =>
            Rel32(p)
        }

      def apply[Size<:WordDoubleSize](operand: ModRMEncodableOperand with Size): X86Operation =
        RM16(operand)

      object Far {
        def apply[Size<:WordDoubleSize](farPointer: FarPointer[Size] with FarPointerSize[Size]): Static with FarPointerOperation[Size] =
          Ptr1616(farPointer)

        def apply(pointer: MemoryLocation with WordDoubleSize): X86Operation =
          M1616(pointer)
      }

      // TODO: this ignores the rel16 option
      def apply(targetLabel: Label): LongJumpOperation = {
        new LongJumpOperation(opcode, mnemonic, targetLabel, 4) {
          override def encodableForLongPointer(offset: Int): Resource with UnlabeledEncodable =
            Rel32(LongPointer.protectedMode(offset))
        }
      }
    }

    object Jump extends ShortOrLongRelativeJumpProtected(0xEB.toByte :: Nil, 0xE9.toByte :: Nil, "jmp") {

      def apply[Size<:WordDoubleSize](operand: ModRMEncodableOperand with Size): X86Operation =
        RM16(operand)

      object Far {
        def apply[Size<:WordDoubleSize](farPointer: FarPointer[Size] with FarPointerSize[Size]): Static with FarPointerOperation[Size] =
          Ptr1616(farPointer)

        def apply(pointer: MemoryLocation with WordDoubleSize): X86Operation =
          M1616(pointer)
      }

    }
    object JumpIfAbove extends ShortOrLongRelativeJumpProtected(Seq(0x77.toByte), Seq(0x0F.toByte, 0x87.toByte), "ja")
    object JumpIfAboveOrEqual extends ShortOrLongRelativeJumpProtected(Seq(0x73.toByte), Seq(0x0F.toByte, 0x83.toByte), "jae")
    object JumpIfBelow extends ShortOrLongRelativeJumpProtected(Seq(0x72.toByte), Seq(0x0F.toByte, 0x82.toByte), "jb")
    object JumpIfBelowOrEqual extends ShortOrLongRelativeJumpProtected(Seq(0x76.toByte), Seq(0x0F.toByte, 0x86.toByte), "jbe")
    object JumpIfCarry extends ShortOrLongRelativeJumpProtected(Seq(0x72.toByte), Seq(0x0F.toByte, 0x82.toByte), "jc")
    object JumpIfCountZero extends ShortRelativeJump(Seq(0xE3.toByte), "jcx") with HasOperandSizePrefixRequirements {
      override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = Jump.operandSizePrefixRequirement
    }
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

  trait LongOperations extends ReturnOperations {
    self: HasOperandSizePrefixRequirements =>

    abstract class ShortOrLongRelativeJumpLong(shortOpcode: Seq[Byte], longOpcode: Seq[Byte], mnemonic: String)
      extends ShortOrLongRelativeJumpI386(shortOpcode, longOpcode, mnemonic) with HasOperandSizePrefixRequirements {

      override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = LongOperations.this.operandSizePrefixRequirement

      def apply(nearPointer: NearPointer with ByteDoubleSize): X86Operation =
        nearPointer match {
          case p: NearPointer with ByteSize =>
            Rel8(p)
          case p: NearPointer with DoubleWordSize =>
            Rel32(p)
        }

      def apply(targetLabel: Label): NearJumpOperation[DoubleWordSize] = {
        new NearJumpOperation[DoubleWordSize](shortOpcode, longOpcode, mnemonic, targetLabel, 4) {
          override def encodableForShortPointer(offset: Byte): Resource with UnlabeledEncodable =
            Rel8(ShortPointer(offset))

          override def encodableForLongPointer(offset: Int): Resource with UnlabeledEncodable =
            Rel32(LongPointer.protectedMode(offset))
        }
      }
    }

    object Call extends CallOperations with HasOperandSizePrefixRequirements {
      override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = LongOperations.this.operandSizePrefixRequirement

      def apply(nearPointer: NearPointer with DoubleWordSize): X86Operation =
        Rel32(nearPointer)

      def apply(operand: ModRMEncodableOperand with QuadWordSize): X86Operation =
        RM16(operand)

      object Far {
        def apply(pointer: MemoryLocation with WordDoubleQuadSize): X86Operation =
          M1616(pointer)
      }

      def apply(targetLabel: Label): LongJumpOperation = {
        new LongJumpOperation(opcode, mnemonic, targetLabel, 4) {
          override def encodableForLongPointer(offset: Int): Resource with UnlabeledEncodable =
            Rel32(LongPointer.protectedMode(offset))
        }
      }
    }

    object Jump extends ShortOrLongRelativeJumpLong(0xEB.toByte :: Nil, 0xE9.toByte :: Nil, "jmp") {

      def apply(operand: ModRMEncodableOperand with QuadWordSize): X86Operation =
        RM16(operand)

      def rm(operand: ModRMEncodableOperand with QuadWordSize): X86Operation =
        RM16(operand)

      object Far {
        def apply(pointer: MemoryLocation with WordDoubleQuadSize): X86Operation =
          M1616(pointer)
      }
    }
    object JumpIfAbove extends ShortOrLongRelativeJumpLong(Seq(0x77.toByte), Seq(0x0F.toByte, 0x87.toByte), "ja")
    object JumpIfAboveOrEqual extends ShortOrLongRelativeJumpLong(Seq(0x73.toByte), Seq(0x0F.toByte, 0x83.toByte), "jae")
    object JumpIfBelow extends ShortOrLongRelativeJumpLong(Seq(0x72.toByte), Seq(0x0F.toByte, 0x82.toByte), "jb")
    object JumpIfBelowOrEqual extends ShortOrLongRelativeJumpLong(Seq(0x76.toByte), Seq(0x0F.toByte, 0x86.toByte), "jbe")
    object JumpIfCarry extends ShortOrLongRelativeJumpLong(Seq(0x72.toByte), Seq(0x0F.toByte, 0x82.toByte), "jc")
    object JumpIfCountZero extends ShortRelativeJump(Seq(0xE3.toByte), "jcx") with HasOperandSizePrefixRequirements {
      override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = Jump.operandSizePrefixRequirement
    }
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
