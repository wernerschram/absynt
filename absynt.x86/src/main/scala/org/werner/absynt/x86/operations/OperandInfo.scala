package org.werner.absynt.x86.operations

import org.werner.absynt.x86.RexRequirement
import org.werner.absynt.x86.operands._

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
    new OperandInfo[ValueSize](pointer, operandOrder) with NoOperandSizePrefix //relXX

  def immediate(immediate: ImmediateValue with ValueSize, operandOrder: OperandOrder)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo[_] =
    new OperandInfo[ValueSize](immediate, operandOrder) with NormalOperandSizePrefix  //immXX

  def implicitOperand(operand: Operand with ValueSize, operandOrder: OperandOrder)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo[_] =
    new OperandInfo[ValueSize](operand, operandOrder) with NormalOperandSizePrefix  //XX

  def implicitPort(operand: DataRegister with ValueSize, operandOrder: OperandOrder)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo[_] =
    new OperandInfo[ValueSize](operand, operandOrder) with NoOperandSizePrefix //XX

  def implicitAddress(memoryLocation: memoryaccess.MemoryLocation with ValueSize, operandOrder: OperandOrder)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo[_] =
    new OperandInfo[ValueSize](memoryLocation, operandOrder) with NoOperandSizePrefix {
      override def addressOperands: Set[AddressOperandInfo] =
        memoryLocation.addressOperands
    } //XX

  def encodedRegister(register: GeneralPurposeRegister with ValueSize, operandOrder: OperandOrder)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo[_] =
    new OperandInfo[ValueSize](register, operandOrder) with NormalOperandSizePrefix {
      override def rexRequirements: Set[RexRequirement] = register.rexRequirements(RexRequirement.instanceOpcodeReg)
    } //rX

  def memoryOffset(offset: memoryaccess.MemoryLocation with ValueSize, operandOrder: OperandOrder)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo[_] =
    new OperandInfo[ValueSize](offset, operandOrder) with NoOperandSizePrefix {

      override def addressOperands: Set[AddressOperandInfo] =
        offset.addressOperands
    } //moffsXX

  def rmRegisterOrMemory(rm: ModRMEncodableOperand with ValueSize, operandOrder: OperandOrder, includeRexW: Boolean)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo[_] =
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

  def rmDisplacement(displacement: ImmediateValue with ValueSize, segmentOverride: Option[SegmentRegister])(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): AddressOperandInfo =
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