package assembler.x86.operations

import assembler.x86.operands._
import assembler.x86.{ProcessorMode, RexRequirement}

sealed abstract class OperandInfo(val operand: Operand, val order: OperandInfo.OperandOrder.Value)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement) extends Ordered[OperandInfo] {
  override def toString: String = operand.toString

  override def compare(that: OperandInfo): Int = order compare that.order

  def requiresOperandSize(processorMode: ProcessorMode): Boolean

  def addressOperands: Set[AddressOperandInfo] = Set.empty

  def rexRequirements: Set[RexRequirement] = operand match {
    case _: QuadWordSize => Set(RexRequirement.quadOperand)
    case _ => Set.empty
  }
}

trait OperandSizePrefixRequirement {
  def normalOperand(size: Operand with ValueSize): Boolean
  def pointerOperand(size: Operand with ValueSize): Boolean
}

trait NoOperandSizePrefix {
  self: OperandInfo =>
  override def requiresOperandSize(processorMode: ProcessorMode): Boolean = false
}

trait NormalOperandSizePrefix {
  self: OperandInfo =>
  override def requiresOperandSize(processorMode: ProcessorMode): Boolean =
    (operand, processorMode) match {
      case (_: WordSize, ProcessorMode.Protected | ProcessorMode.Long) => true
      case (_: DoubleWordSize, ProcessorMode.Real) => true
      case _ => false
    }
}

trait PointerOperandSizePrefix {
  self: OperandInfo =>
  override def requiresOperandSize(processorMode: ProcessorMode): Boolean =
    (operand, processorMode) match {
      case (_: FarWordSize, ProcessorMode.Protected | ProcessorMode.Long) => true
      case (_: FarDoubleWordSize, ProcessorMode.Real) => true
      case _ => false
    }
}

object OperandInfo {
  object OperandOrder extends Enumeration {
    type OperandOrder = Value
    val destination, source, third = Value
  }

  import OperandOrder._

  def pointer(pointer: memoryaccess.FarPointer[_], operandOrder: OperandOrder)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo =
    new OperandInfo(pointer, operandOrder) with PointerOperandSizePrefix //ptrXX

  def relative(pointer: memoryaccess.NearPointer, operandOrder: OperandOrder)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo =
    new OperandInfo(pointer, operandOrder) with NoOperandSizePrefix //relXX

  def immediate(immediate: ImmediateValue, operandOrder: OperandOrder)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo =
    new OperandInfo(immediate, operandOrder) with NormalOperandSizePrefix  //immXX

  def implicitOperand(operand: Operand, operandOrder: OperandOrder)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo =
    new OperandInfo(operand, operandOrder) with NormalOperandSizePrefix  //XX

  def implicitPort(operand: DataRegister, operandOrder: OperandOrder)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo =
    new OperandInfo(operand, operandOrder) with NoOperandSizePrefix //XX

  def implicitAddress(memoryLocation: memoryaccess.MemoryLocation, operandOrder: OperandOrder)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo =
    new OperandInfo(memoryLocation, operandOrder) with NoOperandSizePrefix {
      override def addressOperands: Set[AddressOperandInfo] =
        memoryLocation.addressOperands
    } //XX

  def encodedRegister(register: GeneralPurposeRegister, operandOrder: OperandOrder)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo =
    new OperandInfo(register, operandOrder) with NormalOperandSizePrefix {
      override def rexRequirements: Set[RexRequirement] = register.rexRequirements(RexRequirement.instanceOpcodeReg)
    } //rX

  def memoryOffset(offset: memoryaccess.MemoryLocation, operandOrder: OperandOrder)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo =
    new OperandInfo(offset, operandOrder) with NoOperandSizePrefix {

      override def addressOperands: Set[AddressOperandInfo] =
        offset.addressOperands
    } //moffsXX

  def rmRegisterOrMemory(rm: ModRMEncodableOperand, operandOrder: OperandOrder, includeRexW: Boolean)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo =
    new OperandInfo(rm, operandOrder) with NormalOperandSizePrefix {
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

  def rmRegister(register: GeneralPurposeRegister, operandOrder: OperandOrder)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo =
    new OperandInfo(register, operandOrder) with NormalOperandSizePrefix {
      override def rexRequirements: Set[RexRequirement] = super.rexRequirements ++ register.rexRequirements(RexRequirement.instanceOperandR)
    } //rXX

  def rmSegment(register: SegmentRegister, operandOrder: OperandOrder)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandInfo =
    new OperandInfo(register, operandOrder) with NoOperandSizePrefix //SregXX
}

sealed class AddressOperandInfo(val operand: Operand with ValueSize, val segmentOverride: Option[SegmentRegister] = None) {
  override def toString: String = operand.toString

  def requiresAddressSize(processorMode: ProcessorMode): Boolean =
    (operand, processorMode) match {
      case (_: WordSize, ProcessorMode.Protected) => true
      case (_: DoubleWordSize, ProcessorMode.Real | ProcessorMode.Long) => true
      case _ => false
    }
  def rexRequirements: Set[RexRequirement] = Set.empty
}

object AddressOperandInfo {
  def rmIndex[RegisterSize<:ValueSize](register: GeneralPurposeRegister with IndexRegister with RegisterSize, segmentOverride: Option[SegmentRegister]): AddressOperandInfo =
    new AddressOperandInfo(register, segmentOverride) {
      override def rexRequirements: Set[RexRequirement] = register.rexRequirements(RexRequirement.instanceOperandRM)
    }

  def rmBase[RegisterSize<:ValueSize](register: GeneralPurposeRegister with BaseRegisterReference with RegisterSize): AddressOperandInfo =
    new AddressOperandInfo(register)

  def rmDisplacement(displacement: ImmediateValue with ValueSize, segmentOverride: Option[SegmentRegister]): AddressOperandInfo =
    new AddressOperandInfo(displacement, segmentOverride)

  def SIBBase[RegisterSize<:ValueSize](register: GeneralPurposeRegister with SIBBaseRegister with RegisterSize): AddressOperandInfo =
    new AddressOperandInfo(register) {
      override def rexRequirements: Set[RexRequirement] = register.rexRequirements(RexRequirement.instanceBase)
    }

  def SIBIndex[RegisterSize<:ValueSize](register: GeneralPurposeRegister with SIBIndexRegister with RegisterSize, segmentOverride: Option[SegmentRegister]): AddressOperandInfo =
    new AddressOperandInfo(register, segmentOverride) {
      override def rexRequirements: Set[RexRequirement] = register.rexRequirements(RexRequirement.instanceIndex)
    }

  def memoryOffset(offset: memoryaccess.MemoryLocation with ValueSize): AddressOperandInfo =
    new AddressOperandInfo(offset)

}
