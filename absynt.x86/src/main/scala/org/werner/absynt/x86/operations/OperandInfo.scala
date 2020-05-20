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

import org.werner.absynt.x86.{ArchitectureBounds, RexRequirement}
import org.werner.absynt.x86.operands._

case class OperandWithOperandSizePrefixInfo[T<: Operand](operand: T)(implicit val operandSizePrefixRequirement: OperandSizePrefixRequirement)
case class OperandWithAddressSizePrefixInfo[T<: Operand](operand: T)(implicit val addressSizePrefixRequirement: AddressSizePrefixRequirement)
case class OperandWithSizePrefixInfo[T<: Operand](operand: T)(implicit val operandSizePrefixRequirement: OperandSizePrefixRequirement, val addressSizePrefixRequirement: AddressSizePrefixRequirement)

trait OperandSizeInfo {
  self: ArchitectureBounds =>
  implicit def operandWithOperandSizePrefixInfo[T<: Operand](operand: T)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandWithOperandSizePrefixInfo[T] =
    OperandWithOperandSizePrefixInfo(operand)

  implicit def operandWithAddressSizePrefixInfo[T<: Operand](operand: T)(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): OperandWithAddressSizePrefixInfo[T] =
    OperandWithAddressSizePrefixInfo(operand)

  implicit def operandWithSizePrefixInfo[T<: Operand](operand: T)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement, addressSizePrefixRequirement: AddressSizePrefixRequirement): OperandWithSizePrefixInfo[T] =
    OperandWithSizePrefixInfo(operand)

  val noOperandSizePrefixRequirement: OperandSizePrefixRequirement = new OperandSizePrefixRequirement {
    override def normalOperand(size: Operand with ValueSize): Boolean = false
    override def pointerOperand(size: Operand with FarPointerSize[_]): Boolean = false
  }

  val alwaysOperandSizePrefixRequirement: OperandSizePrefixRequirement = new OperandSizePrefixRequirement {
    override def normalOperand(size: Operand with ValueSize): Boolean = true
    override def pointerOperand(size: Operand with FarPointerSize[_]): Boolean = true
  }
}

sealed abstract class OperandInfo[Size<:OperandSize](val operand: Operand with Size, val order: OperandInfo.OperandOrder.Value)(implicit val operandSizePrefixRequirement: OperandSizePrefixRequirement) extends Ordered[OperandInfo[_]] {
  override def toString: String = operand.toString

  override def compare(that: OperandInfo[_]): Int = order compare that.order

  def requiresOperandSize: Boolean

  def addressOperands: Set[AddressOperandInfo] =
    Set.empty

  def rexRequirements: Set[RexRequirement] = operand match {
    case _: QuadWordSize => Set(RexRequirement.quadOperand)
    case _ => Set.empty
  }
}

trait OperandSizePrefixRequirement {
  def normalOperand(size: Operand with ValueSize): Boolean
  def pointerOperand(size: Operand with FarPointerSize[_]): Boolean
}

trait AddressSizePrefixRequirement {
  def normalAddress(size: Operand with ValueSize): Boolean
}

trait NoOperandSizePrefix {
  self: OperandInfo[_] =>
  override def requiresOperandSize: Boolean = false
}

trait NormalOperandSizePrefix {
  self: OperandInfo[ValueSize] =>
  override def requiresOperandSize: Boolean =
    self.operandSizePrefixRequirement.normalOperand(operand)
}

trait PointerOperandSizePrefix {
  self: OperandInfo[FarPointerSize[_]] =>
  override def requiresOperandSize: Boolean =
    self.operandSizePrefixRequirement.pointerOperand(operand)
}

object OperandInfo {
  object OperandOrder extends Enumeration {
    type OperandOrder = Value
    val destination, source, third = Value
  }

  import OperandOrder._

  def pointer(pointer: memoryaccess.FarPointer[_] with FarPointerSize[_], operandOrder: OperandOrder)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo[_] =
    new OperandInfo[FarPointerSize[_]](pointer, operandOrder) with PointerOperandSizePrefix //ptrXX

  def relative(pointer: memoryaccess.NearPointer with ValueSize, operandOrder: OperandOrder)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo[_] =
    new OperandInfo[ValueSize](pointer, operandOrder) with NormalOperandSizePrefix //relXX

  def immediate(immediate: ImmediateValue[_] with ValueSize, operandOrder: OperandOrder)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo[_] =
    new OperandInfo[ValueSize](immediate, operandOrder) with NormalOperandSizePrefix  //immXX

  def implicitOperand(operand: Operand with ValueSize, operandOrder: OperandOrder)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo[_] =
    new OperandInfo[ValueSize](operand, operandOrder) with NormalOperandSizePrefix  //XX

  def implicitPort(operand: DataRegister with ValueSize, operandOrder: OperandOrder)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo[_] =
    new OperandInfo[ValueSize](operand, operandOrder) with NoOperandSizePrefix //XX

  def implicitAddress(memoryLocation: memoryaccess.MemoryLocation with ValueSize, operandOrder: OperandOrder)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement,  addressSizePrefixRequirement: AddressSizePrefixRequirement): OperandInfo[_] =
    new OperandInfo[ValueSize](memoryLocation, operandOrder) with NormalOperandSizePrefix {
      override def addressOperands: Set[AddressOperandInfo] =
        memoryLocation.addressOperands
    } //XX

  def encodedRegister(register: GeneralPurposeRegister with ValueSize, operandOrder: OperandOrder)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo[_] =
    new OperandInfo[ValueSize](register, operandOrder) with NormalOperandSizePrefix {
      override def rexRequirements: Set[RexRequirement] = register.rexRequirements(RexRequirement.instanceOpcodeReg)
    } //rX

  def memoryOffset(offset: memoryaccess.MemoryLocation with ValueSize, operandOrder: OperandOrder)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement,  addressSizePrefixRequirement: AddressSizePrefixRequirement): OperandInfo[_] =
    new OperandInfo[ValueSize](offset, operandOrder) with NoOperandSizePrefix {

      override def addressOperands: Set[AddressOperandInfo] =
        offset.addressOperands
    } //moffsXX

  def rmRegisterOrMemory(rm: ModRMEncodableOperand with ValueSize, operandOrder: OperandOrder, includeRexW: Boolean)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement, addressSizePrefixRequirement: AddressSizePrefixRequirement): OperandInfo[_] =
    new OperandInfo[ValueSize](rm, operandOrder) with NormalOperandSizePrefix {
      override def addressOperands: Set[AddressOperandInfo] = rm match {
        case l: memoryaccess.MemoryLocation => l.addressOperands
        case _ => Set.empty
      }

      override def rexRequirements: Set[RexRequirement] = {
        val operandRequirements = rm.rexRequirements(RexRequirement.instanceOperandRM)
        if (includeRexW)
          super.rexRequirements ++ operandRequirements
        else
          operandRequirements
      }
    }//r/mXX

  def rmRegister(register: GeneralPurposeRegister with ValueSize, operandOrder: OperandOrder)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo[_] =
    new OperandInfo[ValueSize](register, operandOrder) with NormalOperandSizePrefix {
      override def rexRequirements: Set[RexRequirement] = super.rexRequirements ++ register.rexRequirements(RexRequirement.instanceOperandR)
    } //rXX

  def rmSegment(register: SegmentRegister with ValueSize, operandOrder: OperandOrder)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo[_] =
    new OperandInfo[ValueSize](register, operandOrder) with NoOperandSizePrefix //SregXX
}

sealed class AddressOperandInfo(val operand: Operand with ValueSize, val segmentOverride: Option[SegmentRegister] = None)(implicit val addressSizePrefixRequirement: AddressSizePrefixRequirement) {
  override def toString: String = operand.toString

  def requiresAddressSize: Boolean =
    addressSizePrefixRequirement.normalAddress(operand)

  def rexRequirements: Set[RexRequirement] = Set.empty
}

object AddressOperandInfo {
  def rmIndex[RegisterSize<:ValueSize](register: GeneralPurposeRegister with IndexRegister with RegisterSize, segmentOverride: Option[SegmentRegister])(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): AddressOperandInfo =
    new AddressOperandInfo(register, segmentOverride) {
      override def rexRequirements: Set[RexRequirement] = register.rexRequirements(RexRequirement.instanceOperandRM)
    }

  def rmBase[RegisterSize<:ValueSize](register: GeneralPurposeRegister with BaseRegisterReference with RegisterSize)(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): AddressOperandInfo =
    new AddressOperandInfo(register)

  def rmDisplacement(displacement: ImmediateValue[_] with ValueSize, segmentOverride: Option[SegmentRegister])(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): AddressOperandInfo =
    new AddressOperandInfo(displacement, segmentOverride)

  def SIBBase[RegisterSize<:ValueSize](register: GeneralPurposeRegister with SIBBaseRegister with RegisterSize)(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): AddressOperandInfo =
    new AddressOperandInfo(register) {
      override def rexRequirements: Set[RexRequirement] = register.rexRequirements(RexRequirement.instanceBase)
    }

  def SIBIndex[RegisterSize<:ValueSize](register: GeneralPurposeRegister with SIBIndexRegister with RegisterSize, segmentOverride: Option[SegmentRegister])(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): AddressOperandInfo =
    new AddressOperandInfo(register, segmentOverride) {
      override def rexRequirements: Set[RexRequirement] = register.rexRequirements(RexRequirement.instanceIndex)
    }

  def memoryOffset(offset: memoryaccess.MemoryLocation with ValueSize)(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): AddressOperandInfo =
    new AddressOperandInfo(offset)

}
