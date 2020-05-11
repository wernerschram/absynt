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

package org.werner.absynt.x86

import org.werner.absynt.x86.instructions._
import org.werner.absynt.x86.operands._
import org.werner.absynt.x86.operands.memoryaccess._
import org.werner.absynt.x86.operations.{AddressSizePrefixRequirement, OperandSizePrefixRequirement, OperandWithAddressSizePrefixInfo, OperandWithOperandSizePrefixInfo, OperandWithSizePrefixInfo}
import org.werner.absynt.x86.operands.Register.I8086Registers

trait HasOperandSizePrefixRequirements {
  implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement
}

trait HasNoOperandSizePrefixRequirements extends HasOperandSizePrefixRequirements {
  override implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = new OperandSizePrefixRequirement {
    override def normalOperand(size: Operand with ValueSize): Boolean = false
    override def pointerOperand(size: Operand with FarPointerSize[_]): Boolean = false
  }
}

trait HasAddressSizePrefixRequirements {
  implicit def addressSizePrefixRequirement: AddressSizePrefixRequirement
}


trait ArchitectureBounds {
  type MaxValueSize <: ValueSize
  type MaxWideSize <: WordDoubleQuadSize

  implicit def operandWithOperandSizePrefixInfo[T<: Operand](operand: T)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement): OperandWithOperandSizePrefixInfo[T] =
    OperandWithOperandSizePrefixInfo(operand)

  implicit def operandWithAddressSizePrefixInfo[T<: Operand](operand: T)(implicit addressSizePrefixRequirement: AddressSizePrefixRequirement): OperandWithAddressSizePrefixInfo[T] =
    OperandWithAddressSizePrefixInfo(operand)

  implicit def operandWithSizePrefixInfo[T<: Operand](operand: T)(implicit operandSizePrefixRequirement: OperandSizePrefixRequirement, addressSizePrefixRequirement: AddressSizePrefixRequirement): OperandWithSizePrefixInfo[T] =
    OperandWithSizePrefixInfo(operand)

  def noOperandSizePrefixRequirement: OperandSizePrefixRequirement = new OperandSizePrefixRequirement {
    override def normalOperand(size: Operand with ValueSize): Boolean = false
    override def pointerOperand(size: Operand with FarPointerSize[_]): Boolean = false
  }
}

sealed abstract class ProcessorMode
extends ImmediateValue.I8086Implicits
  with HasOperandSizePrefixRequirements
  with HasAddressSizePrefixRequirements
  with MemoryAddress.I8086Implicits
  with RegisterMemoryLocation.I8086Implicits
  with RegisterMemoryLocation.Operations
  with FarPointer.I8086Implicits
{

  def pointer(location: Long): ImmediateValue with WordDoubleQuadSize
  def shortPointer(location: Byte): NearPointer with ByteSize = ShortPointer(location)
  def wordPointer(location: Int): NearPointer with WordSize = LongPointer.realMode(location)
  def doubleWordPointer(location: Int): NearPointer with DoubleWordSize = LongPointer.protectedMode(location)
}

object ProcessorMode {
  trait LegacyBounds extends ArchitectureBounds {
    type MaxValueSize = ByteWordSize
    type MaxWideSize = WordSize
  }

  object Legacy extends ProcessorMode
    with LegacyBounds
    with I8086Registers
    with Move.LegacyOperations
    with BasicInteraction.LegacyOperations
    with DivideMultiply.Operations
    with Interrupt.LegacyRealProtectedOperations
    with IO.LegacyOperations
    with Jump.LegacyOperations
    with Stack.LegacyOperations
    with String.LegacyOperations
    with Test.LegacyOperations
    with Adjust.Operations
    with IncrementDecrement.LegacyOperations
    with Flags.Operations
  {

    implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = new OperandSizePrefixRequirement {
      override def normalOperand(size: Operand with ValueSize): Boolean = false
      override def pointerOperand(size: Operand with FarPointerSize[_]): Boolean = false
    }

    implicit def addressSizePrefixRequirement: AddressSizePrefixRequirement =
      (_: Operand with ValueSize) => false

    override def pointer(location: Long): ImmediateValue with WordDoubleQuadSize = location.toShort
  }

  trait I386Bounds extends ArchitectureBounds {
    type MaxValueSize = ByteWordDoubleSize
    type MaxWideSize = WordDoubleSize
  }

  trait RealBounds extends I386Bounds

  object Real extends ProcessorMode
    with RealBounds
    with Register.I386Registers
    with ImmediateValue.I386Implicits
    with MemoryAddress.I386Implicits
    with RegisterMemoryLocation.I386Implicits
    with SIBMemoryLocation.I386Operations
    with FarPointer.I386Implicits
    with Register.I386GenericRegisters
    with Move.RealOperations
    with BasicInteraction.RealOperations
    with DivideMultiply.Operations
    with Interrupt.LegacyRealProtectedOperations
    with IO.RealOperations
    with Jump.RealOperations
    with Stack.RealOperations
    with String.RealOperations
    with Test.RealOperations
    with Adjust.Operations
    with IncrementDecrement.RealOperations
    with Flags.Operations
  {

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
  }

  trait ProtectedBounds extends I386Bounds

  object Protected extends ProcessorMode
    with ProtectedBounds
    with Register.I386Registers
    with ImmediateValue.I386Implicits
    with MemoryAddress.I386Implicits
    with RegisterMemoryLocation.I386Implicits
    with SIBMemoryLocation.I386Operations
    with FarPointer.I386Implicits
    with Register.I386GenericRegisters
    with Move.ProtectedOperations
    with BasicInteraction.ProtectedOperations
    with DivideMultiply.Operations
    with Interrupt.LegacyRealProtectedOperations
    with IO.ProtectedOperations
    with Jump.ProtectedOperations
    with Stack.ProtectedOperations
    with String.ProtectedOperations
    with System.ProtectedOperations
    with Test.ProtectedOperations
    with Adjust.Operations
    with IncrementDecrement.ProtectedOperations
    with Flags.Operations
  {
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
  }
  trait LongBounds extends ArchitectureBounds {
    type MaxValueSize = ValueSize
    type MaxWideSize = WordDoubleQuadSize
  }

  object Long extends ProcessorMode
    with LongBounds
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
    with DivideMultiply.Operations
    with Interrupt.LongOperations
    with IO.LongOperations
    with Jump.LongOperations
    with Stack.LongOperations
    with String.LongOperations
    with System.LongOperations
    with Test.LongOperations
    with IncrementDecrement.LongOperations
    with Flags.Operations
  {
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
  }
}
