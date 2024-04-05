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
import org.werner.absynt.x86.operands.*
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder.OperandOrder
import org.werner.absynt.x86.operations.*
import scala.language.implicitConversions

object Shift {

  trait Operations {
    self: ArchitectureBounds & ProcessorMode & OperandSizeInfo & ImmediateValue.I8086Implicits =>

    protected def OneToRM8(operand: ModRMEncodableOperand & ByteSize, extensionCode: Byte, mnemonic: String): X86Operation =
      new ModRM[ByteSize](operand, 0xD0.toByte :: Nil, extensionCode, mnemonic, OperandOrder.destination, true)
        with NoDisplacement
        with NoImmediate
        with ExtraOperands(OperandInfo.implicitOperand(1.toByte, OperandOrder.source)(using noOperandSizePrefixRequirement))

    protected def CLToRM8(operand: ModRMEncodableOperand & ByteSize, extensionCode: Byte, mnemonic: String): X86Operation =
      new ModRM[ByteSize](operand, 0xD2.toByte :: Nil, extensionCode, mnemonic, OperandOrder.destination, true)
        with NoDisplacement
        with NoImmediate
        with ExtraOperands(OperandInfo.implicitOperand(Count.LowByte, OperandOrder.source)(using noOperandSizePrefixRequirement))

    protected def Imm8ToRM8(immediateValue: ImmediateValue[Byte] & ByteSize, operand: ModRMEncodableOperand & ByteSize, extensionCode: Byte, mnemonic: String): X86Operation =
      new ModRM[ByteSize](operand, 0xC0.toByte :: Nil, extensionCode, mnemonic, OperandOrder.destination, true)
        with NoDisplacement
        with Immediate[ByteSize](immediateValue, OperandOrder.source)

    protected def OneToRM16(operand: ModRMEncodableOperand & WordDoubleQuadSize, extensionCode: Byte, mnemonic: String): X86Operation =
      new ModRM[WordDoubleQuadSize](operand, 0xD1.toByte :: Nil, extensionCode, mnemonic, OperandOrder.destination, true)
        with NoDisplacement
        with NoImmediate
        with ExtraOperands(OperandInfo.implicitOperand(1.toByte, OperandOrder.source)(using noOperandSizePrefixRequirement))
      

    protected def CLToRM16[Size <: MaxWideSize](operand: ModRMEncodableOperand & Size, extensionCode: Byte, mnemonic: String): X86Operation =
      new ModRM[Size](operand, 0xD3.toByte :: Nil, extensionCode, mnemonic, OperandOrder.destination, true)
        with NoDisplacement
        with NoImmediate
        with ExtraOperands(OperandInfo.implicitOperand(Count.LowByte, OperandOrder.source)(using noOperandSizePrefixRequirement))

    protected def Imm8ToRM16(immediateValue: ImmediateValue[Byte] & ByteSize, operand: ModRMEncodableOperand & WordDoubleQuadSize, extensionCode: Byte, mnemonic: String): X86Operation =
      new ModRM[WordDoubleQuadSize](operand, 0xC1.toByte :: Nil, extensionCode, mnemonic, OperandOrder.destination, true)
        with NoDisplacement
        with Immediate[ByteSize](immediateValue, OperandOrder.source)

    sealed abstract class ShiftInstruction(extensionCode: Byte, mnemonic: String) {
      def apply[Size <: MaxValueSize](immediateValue: ImmediateValue[Byte] & ByteSize, destination: ModRMEncodableOperand & Size): X86Operation = {
        val value = immediateValue.value
        destination match {
          case d: ByteSize if value == 1 => OneToRM8(d, extensionCode, mnemonic)
          case d: WordDoubleQuadSize if value == 1 => OneToRM16(d, extensionCode, mnemonic)
          case d: ByteSize => Imm8ToRM8((value & 7).toByte, d, extensionCode, mnemonic)
          case d: WordSize => Imm8ToRM16((value & 15).toByte, d, extensionCode, mnemonic)
          case d: DoubleWordSize => Imm8ToRM16((value & 31).toByte, d, extensionCode, mnemonic)
          case d: QuadWordSize => Imm8ToRM16((value & 63).toByte, d, extensionCode, mnemonic)
        }
      }

      def apply[Size <: MaxValueSize](countRegister: Count.LowByte.type, destination: ModRMEncodableOperand & Size): X86Operation =
        destination match {
          case d: ByteSize =>
            CLToRM8(d, extensionCode, mnemonic)
          case d: MaxWideSize @unchecked =>
            CLToRM16(d, extensionCode, mnemonic)
        }
    }

    object ShiftArithmeticLeft extends ShiftInstruction(4.toByte, "sal")
    object ShiftArithmeticRight extends ShiftInstruction(7.toByte, "sar")
    object ShiftLogicalLeft extends ShiftInstruction(4.toByte, "shl")
    object ShiftLogicalRight extends ShiftInstruction(5.toByte, "shr")

    object RotateCarryLeft extends ShiftInstruction(2.toByte, "rcl")
    object RotateCarryRight extends ShiftInstruction(3.toByte, "rcr")
    object RotateLeft extends ShiftInstruction(0.toByte, "rol")
    object RotateRight extends ShiftInstruction(1.toByte, "ror")
  }
}
