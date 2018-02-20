package assembler.x86.operations

import assembler.x86.operands._
import assembler.x86.{ProcessorMode, RexRequirement}

sealed abstract class OperandInfo(val operand: Operand, val order: OperandInfo.OperandOrder.Value) extends Ordered[OperandInfo] {
  override def toString: String = operand.toString

  override def compare(that: OperandInfo): Int = order compare that.order

  def requiresOperandSize(processorMode: ProcessorMode): Boolean = false

  def addressOperands: Set[AddressOperandInfo] = Set.empty

  def rexRequirements: Set[RexRequirement] = operand match {
    case f: QuadWordSize => Set(RexRequirement.quadOperand)
    case _ => Set.empty
  }
}

trait OperandSizePrefix {
  self: OperandInfo =>
  override def requiresOperandSize(processorMode: ProcessorMode): Boolean =
    (operand, processorMode) match {
      case (_: WordSize, ProcessorMode.Protected | ProcessorMode.Long) => true
      case (_: DoubleWordSize, ProcessorMode.Real) => true
      case _ => false
    }
}

object OperandInfo {
  object OperandOrder extends Enumeration {
    type OperandOrder = Value
    val destination, source, third = Value
  }

  import OperandOrder._

  def pointer(pointer: memoryaccess.FarPointer, operandOrder: OperandOrder): OperandInfo =
    new OperandInfo(pointer, operandOrder) {
      override def requiresOperandSize(processorMode: ProcessorMode): Boolean = pointer match {
        case _: FarDoubleWordSize => processorMode != ProcessorMode.Real
        case _: FarWordSize => processorMode == ProcessorMode.Real
      }
    } //ptrXX

  def relative(pointer: memoryaccess.NearPointer, operandOrder: OperandOrder): OperandInfo =
    new OperandInfo(pointer, operandOrder) { } //relXX

  def immediate(immediate: ImmediateValue, operandOrder: OperandOrder): OperandInfo =
    new OperandInfo(immediate, operandOrder) with OperandSizePrefix  { } //immXX

  def implicitOperand(operand: Operand, operandOrder: OperandOrder): OperandInfo =
    new OperandInfo(operand, operandOrder) with OperandSizePrefix { } //XX

  def implicitPort(operand: Operand, operandOrder: OperandOrder): OperandInfo =
    new OperandInfo(operand, operandOrder) { } //XX

  def implicitAddress(operand: Operand, operandOrder: OperandOrder): OperandInfo =
    new OperandInfo(operand, operandOrder) {
      override def addressOperands: Set[AddressOperandInfo] = operand match {
        case l: memoryaccess.MemoryLocation => l.addressOperands
        case _ => Set.empty
      }
    } //XX

  def encodedRegister(register: GeneralPurposeRegister, operandOrder: OperandOrder): OperandInfo =
    new OperandInfo(register, operandOrder) with OperandSizePrefix {
      override def rexRequirements: Set[RexRequirement] = register match {
        case r: GeneralPurposeRexRegister => Set(RexRequirement.instanceOpcodeReg)
        case _ => Set.empty
      }

    } //rX

  def memoryOffset(offset: memoryaccess.MemoryLocation, operandOrder: OperandOrder): OperandInfo =
    new OperandInfo(offset, operandOrder) {

      override def addressOperands: Set[AddressOperandInfo] = operand match {
        case l: memoryaccess.MemoryLocation => l.addressOperands
        case _ => Set.empty
      }
    } //moffsXX

  def rmRegisterOrMemory(rm: ModRMEncodableOperand, operandOrder: OperandOrder, includeRexW: Boolean): OperandInfo =
    new OperandInfo(rm, operandOrder) with OperandSizePrefix {
      override def addressOperands: Set[AddressOperandInfo] = rm match {
        case l: memoryaccess.MemoryLocation => l.addressOperands
        case _ => Set.empty
      }

      override def rexRequirements: Set[RexRequirement] = {
        val operandRequirements = rm match {
          case _: GeneralPurposeRexRegister =>
            Set(RexRequirement.instanceOperandRM)
          case _ => Set.empty[RexRequirement]
        }
        if (includeRexW)
          super.rexRequirements ++ operandRequirements
        else
          operandRequirements
      }
    }//r/mXX

  def rmRegister(register: GeneralPurposeRegister, operandOrder: OperandOrder): OperandInfo =
    new OperandInfo(register, operandOrder) with OperandSizePrefix {
      override def rexRequirements: Set[RexRequirement] = register match {
        case _: GeneralPurposeRexRegister => super.rexRequirements + RexRequirement.instanceOperandR
        case _ => super.rexRequirements
      }
    } //rXX

  def rmSegment(register: SegmentRegister, operandOrder: OperandOrder): OperandInfo =
    new OperandInfo(register, operandOrder) {} //SregXX

}

sealed abstract class AddressOperandInfo(val operand: Operand with ValueSize) {
  override def toString: String = operand.toString

  def requiresAddressSize(processorMode: ProcessorMode): Boolean = false

  def rexRequirements: Set[RexRequirement] = Set.empty
}

trait AddressSizePrefix {
  self: AddressOperandInfo =>
  override def requiresAddressSize(processorMode: ProcessorMode): Boolean =
    (operand, processorMode) match {
      case (_: WordSize, ProcessorMode.Protected) => true
      case (_: DoubleWordSize, ProcessorMode.Real | ProcessorMode.Long) => true
      case _ => false
    }
}

object AddressOperandInfo {
  def rmIndex(register: GeneralPurposeRegister with IndexRegister): AddressOperandInfo =
    new AddressOperandInfo(register) with AddressSizePrefix {
      override def rexRequirements: Set[RexRequirement] = register match {
        case _: GeneralPurposeRexRegister =>
          super.rexRequirements + RexRequirement.instanceOperandRM
        case _ =>
          super.rexRequirements
      }
    }

  def rmBase(register: GeneralPurposeRegister with BaseRegisterReference): AddressOperandInfo =
    new AddressOperandInfo(register) with AddressSizePrefix

  def rmDisplacement(displacement: ImmediateValue): AddressOperandInfo =
    new AddressOperandInfo(displacement) with AddressSizePrefix

  def SIBBase(register: GeneralPurposeRegister with SIBBaseRegister): AddressOperandInfo =
    new AddressOperandInfo(register) with AddressSizePrefix {
      override def rexRequirements: Set[RexRequirement] = register match {
        case _: GeneralPurposeRexRegister =>
          Set(RexRequirement.instanceBase)
        case _ =>
          Set.empty
      }
    }

  def SIBIndex(register: GeneralPurposeRegister with SIBIndexRegister): AddressOperandInfo =
    new AddressOperandInfo(register) with AddressSizePrefix {
      override def rexRequirements: Set[RexRequirement] = register match {
        case _: GeneralPurposeRexRegister =>
          Set(RexRequirement.instanceIndex)
        case _ =>
          Set.empty
      }
    }

  def memoryOffset(offset: memoryaccess.MemoryLocation with ValueSize): AddressOperandInfo =
    new AddressOperandInfo(offset) with AddressSizePrefix

}
