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

package org.werner.absynt.x86.operations

import org.werner.absynt.x86.{ArchitectureBounds, ProcessorMode, RexRequirement}
import org.werner.absynt.x86.operands.*
import scala.language.implicitConversions

case class OperandWithOperandSizePrefixInfo[T<: Operand](operand: T)(using val operandSizePrefixRequirement: OperandSizePrefixRequirement)
case class OperandWithAddressSizePrefixInfo[T<: Operand](operand: T)(using val addressSizePrefixRequirement: AddressSizePrefixRequirement)
case class OperandWithSizePrefixInfo[T<: Operand](operand: T)(using val operandSizePrefixRequirement: OperandSizePrefixRequirement, val addressSizePrefixRequirement: AddressSizePrefixRequirement)

trait OperandSizeInfo {
  self: ArchitectureBounds & ProcessorMode =>

  extension [T <: Operand](operand: T) {
    def withSizePrefixRequirement(operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandWithOperandSizePrefixInfo[T] =
      OperandWithOperandSizePrefixInfo(operand)(using operandSizePrefixRequirement)
  }

  given [T <: Operand]: Conversion[T, OperandWithOperandSizePrefixInfo[T]] with
    override def apply(operand: T): OperandWithOperandSizePrefixInfo[T] =
      OperandWithOperandSizePrefixInfo(operand)

  given [T <: Operand]: Conversion[T, OperandWithAddressSizePrefixInfo[T]] with
    override def apply(operand: T): OperandWithAddressSizePrefixInfo[T] =
      OperandWithAddressSizePrefixInfo(operand)

  given [T <: Operand]: Conversion[T, OperandWithSizePrefixInfo[T]] with
    override def apply(operand: T): OperandWithSizePrefixInfo[T] =
      OperandWithSizePrefixInfo(operand)

  val noOperandSizePrefixRequirement: OperandSizePrefixRequirement = new OperandSizePrefixRequirement {
    override def normalOperand(size: Operand & ValueSize): Boolean = false
    override def pointerOperand(size: Operand & FarPointerSize[?]): Boolean = false
  }

  val alwaysOperandSizePrefixRequirement: OperandSizePrefixRequirement = new OperandSizePrefixRequirement {
    override def normalOperand(size: Operand & ValueSize): Boolean = true
    override def pointerOperand(size: Operand & FarPointerSize[?]): Boolean = true
  }
}

sealed abstract class OperandInfo[Size<:OperandSize](val operand: Operand & Size, val order: OperandInfo.OperandOrder.Value)(using val operandSizePrefixRequirement: OperandSizePrefixRequirement) extends Ordered[OperandInfo[?]] {
  override def toString: String = operand.toString

  override def compare(that: OperandInfo[?]): Int = order compare that.order

  def requiresOperandSize: Boolean

  def addressOperands: Set[AddressOperandInfo] =
    Set.empty

  def rexRequirements: Set[RexRequirement] = operand match {
    case _: QuadWordSize => Set(RexRequirement.quadOperand)
    case _ => Set.empty
  }
}

trait OperandSizePrefixRequirement {
  def normalOperand(size: Operand & ValueSize): Boolean
  def pointerOperand(size: Operand & FarPointerSize[?]): Boolean
}

trait AddressSizePrefixRequirement {
  def normalAddress(size: Operand & ValueSize): Boolean
}

trait NoOperandSizePrefix {
  self: OperandInfo[?] =>
  override def requiresOperandSize: Boolean = false
}

trait NormalOperandSizePrefix {
  self: OperandInfo[ValueSize] =>
  override def requiresOperandSize: Boolean =
    self.operandSizePrefixRequirement.normalOperand(operand)
}

trait PointerOperandSizePrefix {
  self: OperandInfo[FarPointerSize[?]] =>
  override def requiresOperandSize: Boolean =
    self.operandSizePrefixRequirement.pointerOperand(operand)
}

object OperandInfo {
  object OperandOrder extends Enumeration {
    type OperandOrder = Value
    val destination, source, third = Value
  }

  import OperandOrder._

  def pointer(pointer: memoryaccess.FarPointer[?] & FarPointerSize[?], operandOrder: OperandOrder)(using operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo[?] =
    new OperandInfo[FarPointerSize[?]](pointer, operandOrder) with PointerOperandSizePrefix //ptrXX

  def relative(pointer: memoryaccess.NearPointer & ValueSize, operandOrder: OperandOrder)(using operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo[?] =
    new OperandInfo[ValueSize](pointer, operandOrder) with NormalOperandSizePrefix //relXX

  def immediate(immediate: ImmediateValue[?] & ValueSize, operandOrder: OperandOrder)(using operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo[?] =
    new OperandInfo[ValueSize](immediate, operandOrder) with NormalOperandSizePrefix  //immXX

  def implicitOperand(operand: Operand & ValueSize, operandOrder: OperandOrder)(using operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo[?] =
    new OperandInfo[ValueSize](operand, operandOrder) with NormalOperandSizePrefix  //XX

  def implicitPort(operand: DataRegister & ValueSize, operandOrder: OperandOrder)(using operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo[?] =
    new OperandInfo[ValueSize](operand, operandOrder) with NoOperandSizePrefix //XX

  def implicitAddress(memoryLocation: memoryaccess.MemoryLocation & ValueSize, operandOrder: OperandOrder)(using operandSizePrefixRequirement: OperandSizePrefixRequirement,  addressSizePrefixRequirement: AddressSizePrefixRequirement): OperandInfo[?] =
    new OperandInfo[ValueSize](memoryLocation, operandOrder) with NormalOperandSizePrefix {
      override def addressOperands: Set[AddressOperandInfo] =
        memoryLocation.addressOperands
    } //XX

  def encodedRegister(register: GeneralPurposeRegister & ValueSize, operandOrder: OperandOrder)(using operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo[?] =
    new OperandInfo[ValueSize](register, operandOrder) with NormalOperandSizePrefix {
      override def rexRequirements: Set[RexRequirement] = register.rexRequirements(RexRequirement.instanceOpcodeReg)
    } //rX

  def memoryOffset(offset: memoryaccess.MemoryLocation & ValueSize, operandOrder: OperandOrder)(using operandSizePrefixRequirement: OperandSizePrefixRequirement,  addressSizePrefixRequirement: AddressSizePrefixRequirement): OperandInfo[?] =
    new OperandInfo[ValueSize](offset, operandOrder) with NoOperandSizePrefix {

      override def addressOperands: Set[AddressOperandInfo] =
        offset.addressOperands
    } //moffsXX

  def rmRegisterOrMemory(rm: ModRMEncodableOperand & ValueSize, operandOrder: OperandOrder, includeRexW: Boolean)(using operandSizePrefixRequirement: OperandSizePrefixRequirement, addressSizePrefixRequirement: AddressSizePrefixRequirement): OperandInfo[?] =
    new OperandInfo[ValueSize](rm, operandOrder) with NormalOperandSizePrefix {
      override def addressOperands: Set[AddressOperandInfo] = rm match {
        case l: memoryaccess.MemoryLocation => l.addressOperands
        case _ => Set.empty
      }

      override def rexRequirements: Set[RexRequirement] = {
        val operandRequirements = rm.rexRequirements(RexRequirement.instanceOperandRM)
        if includeRexW then
          super.rexRequirements ++ operandRequirements
        else
          operandRequirements
      }
    }//r/mXX

  def rmRegister(register: GeneralPurposeRegister & ValueSize, operandOrder: OperandOrder)(using operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo[?] =
    new OperandInfo[ValueSize](register, operandOrder) with NormalOperandSizePrefix {
      override def rexRequirements: Set[RexRequirement] = super.rexRequirements ++ register.rexRequirements(RexRequirement.instanceOperandR)
    } //rXX

  def rmSegment(register: SegmentRegister & ValueSize, operandOrder: OperandOrder)(using operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo[?] =
    new OperandInfo[ValueSize](register, operandOrder) with NoOperandSizePrefix //SregXX
}

sealed class AddressOperandInfo(val operand: Operand & ValueSize, val segmentOverride: Option[SegmentRegister] = None)(using val addressSizePrefixRequirement: AddressSizePrefixRequirement) {
  override def toString: String = operand.toString

  def requiresAddressSize: Boolean =
    addressSizePrefixRequirement.normalAddress(operand)

  def rexRequirements: Set[RexRequirement] = Set.empty
}

object AddressOperandInfo {
  def rmIndex[RegisterSize<:ValueSize](register: GeneralPurposeRegister & IndexRegister & ValueSize, segmentOverride: Option[SegmentRegister])(using addressSizePrefixRequirement: AddressSizePrefixRequirement): AddressOperandInfo =
    new AddressOperandInfo(register, segmentOverride) {
      override def rexRequirements: Set[RexRequirement] = register.rexRequirements(RexRequirement.instanceOperandRM)
    }

  def rmBase[RegisterSize<:ValueSize](register: GeneralPurposeRegister & ReferenceBaseRegister & RegisterSize)(using addressSizePrefixRequirement: AddressSizePrefixRequirement): AddressOperandInfo =
    new AddressOperandInfo(register) {
      override def rexRequirements: Set[RexRequirement] = register.rexRequirements(RexRequirement.instanceBase)
    }

  def rmDisplacement(displacement: ImmediateValue[?] & ValueSize, segmentOverride: Option[SegmentRegister])(using addressSizePrefixRequirement: AddressSizePrefixRequirement): AddressOperandInfo =
    new AddressOperandInfo(displacement, segmentOverride)

  def SIBBase[RegisterSize<:ValueSize](register: GeneralPurposeRegister & SIBBaseRegister & RegisterSize)(using addressSizePrefixRequirement: AddressSizePrefixRequirement): AddressOperandInfo =
    new AddressOperandInfo(register) {
      override def rexRequirements: Set[RexRequirement] = register.rexRequirements(RexRequirement.instanceBase)
    }

  def SIBIndex[RegisterSize<:ValueSize](register: GeneralPurposeRegister & ProtectedModeIndexRegister & RegisterSize, segmentOverride: Option[SegmentRegister])(using addressSizePrefixRequirement: AddressSizePrefixRequirement): AddressOperandInfo =
    new AddressOperandInfo(register, segmentOverride) {
      override def rexRequirements: Set[RexRequirement] = register.rexRequirements(RexRequirement.instanceIndex)
    }

  def memoryOffset(offset: memoryaccess.MemoryLocation & ValueSize)(using addressSizePrefixRequirement: AddressSizePrefixRequirement): AddressOperandInfo =
    new AddressOperandInfo(offset)

}
