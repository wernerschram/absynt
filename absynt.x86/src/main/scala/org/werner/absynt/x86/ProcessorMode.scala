package org.werner.absynt.x86

import org.werner.absynt.x86.instructions._
import org.werner.absynt.x86.operands._
import org.werner.absynt.x86.operands.memoryaccess._
import org.werner.absynt.x86.operations.{AddressSizePrefixRequirement, OperandSizePrefixRequirement}
import org.werner.absynt.x86.operands.Register.I8086Registers

trait HasOperandSizePrefixRequirements {
  implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement
}

trait HasAddressSizePrefixRequirements {
  implicit def addressSizePrefixRequirement: AddressSizePrefixRequirement
}

sealed abstract class ProcessorMode
  extends ImmediateValue.I8086Implicits
  with MemoryAddress.I8086Implicits
  with RegisterMemoryLocation.I8086Implicits
  with RegisterMemoryLocation.Operations
  with FarPointer.I8086Implicits
{
  self: HasOperandSizePrefixRequirements =>
  type LongPointerSize <: WordDoubleSize

  def pointer(location: Long): ImmediateValue with WordDoubleQuadSize
  def shortPointer(location: Byte): NearPointer with ByteSize = ShortPointer(location)
  def longPointer(location: Int): NearPointer with LongPointerSize
}

object ProcessorMode {

  object Legacy extends ProcessorMode
    with HasOperandSizePrefixRequirements
    with I8086Registers
    with Move.LegacyOperations
    with BasicInteraction.LegacyOperations
    with Interrupt.Operations
    with IO.LegacyOperations
    with Jump.LegacyOperations
    with Stack.LegacyOperations
    with StoreString.LegacyOperations
  {
    override type LongPointerSize = WordSize

    implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = new OperandSizePrefixRequirement {
      override def normalOperand(size: Operand with ValueSize): Boolean = false
      override def pointerOperand(size: Operand with FarPointerSize[_]): Boolean = false
    }

    implicit def addressSizePrefixRequirement: AddressSizePrefixRequirement =
      (_: Operand with ValueSize) => false

    override def pointer(location: Long): ImmediateValue with WordDoubleQuadSize = location.toShort
    override def longPointer(location: Int): NearPointer with WordSize = LongPointer.realMode(location)
  }

  object Real extends ProcessorMode
    with HasOperandSizePrefixRequirements
    with Register.I386Registers
    with ImmediateValue.I386Implicits
    with MemoryAddress.I386Implicits
    with RegisterMemoryLocation.I386Implicits
    with SIBMemoryLocation.I386Operations
    with FarPointer.I386Implicits
    with Register.I386GenericRegisters
    with Move.RealOperations
    with BasicInteraction.RealOperations
    with Interrupt.Operations
    with IO.RealOperations
    with Jump.RealOperations
    with Stack.RealOperations
    with StoreString.RealOperations
  {
    override type LongPointerSize = WordSize

    implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = new OperandSizePrefixRequirement {
      override def normalOperand(size: Operand with ValueSize): Boolean = size match {
        case _: DoubleWordSize => true
        case _ => false
      }
      override def pointerOperand(size: Operand with FarPointerSize[_]): Boolean = size match {
        case _: FarDoubleWordSize => true
        case _ => false
      }
    }

    implicit def addressSizePrefixRequirement: AddressSizePrefixRequirement = {
        case _: DoubleWordSize => true
        case _ => false
      }

    override def pointer(location: Long): ImmediateValue with WordDoubleQuadSize = location.toShort
    override def longPointer(location: Int): NearPointer with WordSize = LongPointer.realMode(location)
  }

  object Protected extends ProcessorMode
    with HasOperandSizePrefixRequirements
    with Register.I386Registers
    with ImmediateValue.I386Implicits
    with MemoryAddress.I386Implicits
    with RegisterMemoryLocation.I386Implicits
    with SIBMemoryLocation.I386Operations
    with FarPointer.I386Implicits
    with Register.I386GenericRegisters
    with Move.ProtectedOperations
    with BasicInteraction.ProtectedOperations
    with Interrupt.Operations
    with IO.ProtectedOperations
    with Jump.ProtectedOperations
    with Stack.ProtectedOperations
    with StoreString.ProtectedOperations
    with System.ProtectedOperations
  {
    override type LongPointerSize = DoubleWordSize

    implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = new OperandSizePrefixRequirement {
      override def normalOperand(size: Operand with ValueSize): Boolean = size match {
        case _: WordSize => true
        case _ => false
      }
      override def pointerOperand(size: Operand with FarPointerSize[_]): Boolean = size match {
        case _: FarWordSize => true
        case _ => false
      }
    }

    implicit def addressSizePrefixRequirement: AddressSizePrefixRequirement = {
      case _: WordSize => true
      case _ => false
    }

    override def pointer(location: Long): ImmediateValue with WordDoubleQuadSize = location.toInt
    override def longPointer(location: Int): NearPointer with DoubleWordSize = LongPointer.protectedMode(location)
  }

  object Long extends ProcessorMode
    with HasOperandSizePrefixRequirements
    with Register.X64Registers
    with ImmediateValue.I386Implicits
    with ImmediateValue.X64Implicits
    with MemoryAddress.I386Implicits
    with MemoryAddress.X64Implicits
    with RegisterMemoryLocation.I386Implicits
    with RegisterMemoryLocation.X64Implicits
    with SIBMemoryLocation.LongOperations
    with FarPointer.I386Implicits
    with Register.I386GenericRegisters
    with Register.X64GenericRegisters
    with Move.LongOperations
    with BasicInteraction.LongOperations
    with Interrupt.Operations
    with IO.LongOperations
    with Jump.LongOperations
    with Stack.LongOperations
    with StoreString.LongOperations
    with System.LongOperations
  {
    override type LongPointerSize = DoubleWordSize

    implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = new OperandSizePrefixRequirement {
      override def normalOperand(size: Operand with ValueSize): Boolean = size match {
        case _: WordSize => true
        case _ => false
      }
      override def pointerOperand(size: Operand with FarPointerSize[_]): Boolean = size match {
        case _: FarWordSize => true
        case _ => false
      }
    }

    implicit def addressSizePrefixRequirement: AddressSizePrefixRequirement = {
      case _: DoubleWordSize => true
      case _ => false
    }

    override def pointer(location: Long): ImmediateValue with WordDoubleQuadSize = location
    def longPointer(location: Int): NearPointer with DoubleWordSize = LongPointer.protectedMode(location)
  }
}
