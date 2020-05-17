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
import org.werner.absynt.x86.operands.Register.I8086Registers
import org.werner.absynt.x86.operands._
import org.werner.absynt.x86.operands.memoryaccess._
import org.werner.absynt.x86.operations.{AddressSizePrefixRequirement, OperandSizeInfo, OperandSizePrefixRequirement}

sealed trait ArchitectureBounds {
  self: ProcessorMode =>
  type MaxValueSize <: ValueSize
  type MaxWideSize <: WordDoubleQuadSize

  implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement
  implicit def addressSizePrefixRequirement: AddressSizePrefixRequirement

}

sealed abstract class ProcessorMode
extends ImmediateValue.I8086Implicits
  with MemoryAddress.I8086Implicits
  with RegisterMemoryLocation.I8086Implicits
  with RegisterMemoryLocation.Operations
  with FarPointer.I8086Implicits
{

  def pointer(location: Long): ImmediateValue[_] with WordDoubleQuadSize
  def shortPointer(location: Byte): NearPointer with ByteSize = ShortPointer(location)
  def wordPointer(location: Int): NearPointer with WordSize = LongPointer.realMode(location)
  def doubleWordPointer(location: Int): NearPointer with DoubleWordSize = LongPointer.protectedMode(location)
}

object ProcessorMode {
  trait LegacyBounds extends ArchitectureBounds {
    self: ProcessorMode =>
    type MaxValueSize = ByteWordSize
    type MaxWideSize = WordSize
  }

  object Legacy extends ProcessorMode
    with OperandSizeInfo
    with LegacyBounds
    with I8086Registers
    with Move.LegacyOperations
    with BasicInteraction.LegacyOperations
    with DivideMultiply.Operations
    with Interrupt.LegacyRealProtectedOperations
    with IO.LegacyOperations
    with Jump.LegacyOperations
    with Stack.LegacyOperations
    with String.Operations
    with Test.LegacyOperations
    with Adjust.Operations
    with IncrementDecrement.LegacyOperations
    with Flags.Operations
    with Shift.Operations
    with Convert.LegacyOperations
  {

    implicit def operandSizePrefixRequirement: OperandSizePrefixRequirement = new OperandSizePrefixRequirement {
      override def normalOperand(size: Operand with ValueSize): Boolean = false
      override def pointerOperand(size: Operand with FarPointerSize[_]): Boolean = false
    }

    implicit def addressSizePrefixRequirement: AddressSizePrefixRequirement =
      (_: Operand with ValueSize) => false

    override def pointer(location: Long): ImmediateValue[Short] with WordDoubleQuadSize = location.toShort
  }

  sealed trait I386Bounds extends ArchitectureBounds {
    self: ProcessorMode =>
    type MaxValueSize = ByteWordDoubleSize
    type MaxWideSize = WordDoubleSize
  }

  object Real extends ProcessorMode
    with OperandSizeInfo
    with I386Bounds
    with Register.I386Registers
    with ImmediateValue.I386Implicits
    with MemoryAddress.I386Implicits
    with RegisterMemoryLocation.I386Implicits
    with SIBMemoryLocation.I386Operations
    with FarPointer.I386Implicits
    with Register.I386GenericRegisters
    with Move.RealOperations
    with BasicInteraction.I386Operations
    with DivideMultiply.Operations
    with Interrupt.LegacyRealProtectedOperations
    with IO.I386Operations
    with Jump.RealOperations
    with Stack.I386Operations
    with String.Operations
    with Test.I386Operations
    with Adjust.Operations
    with IncrementDecrement.I386Operations
    with Flags.Operations
    with Shift.Operations
    with Exchange.Operations
    with Convert.I386Operations
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

    override def pointer(location: Long): ImmediateValue[Short] with WordDoubleQuadSize = location.toShort
  }

  object Protected extends ProcessorMode
    with OperandSizeInfo
    with I386Bounds
    with Register.I386Registers
    with ImmediateValue.I386Implicits
    with MemoryAddress.I386Implicits
    with RegisterMemoryLocation.I386Implicits
    with SIBMemoryLocation.I386Operations
    with FarPointer.I386Implicits
    with Register.I386GenericRegisters
    with Move.ProtectedOperations
    with BasicInteraction.I386Operations
    with DivideMultiply.Operations
    with Interrupt.LegacyRealProtectedOperations
    with IO.I386Operations
    with Jump.ProtectedOperations
    with Stack.I386Operations
    with String.Operations
    with System.ProtectedOperations
    with Test.I386Operations
    with Adjust.Operations
    with IncrementDecrement.I386Operations
    with Flags.Operations
    with Shift.Operations
    with Exchange.Operations
    with Convert.I386Operations
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

    override def pointer(location: Long): ImmediateValue[Int] with WordDoubleQuadSize = location.toInt
  }
  sealed trait LongBounds extends ArchitectureBounds {
    self: ProcessorMode =>

    type MaxValueSize = ValueSize
    type MaxWideSize = WordDoubleQuadSize
  }

  object Long extends ProcessorMode
    with OperandSizeInfo
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
    with String.Operations
    with System.LongOperations
    with Test.LongOperations
    with IncrementDecrement.LongOperations
    with Flags.Operations
    with Shift.Operations
    with Exchange.Operations
    with Convert.LongOperations
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

    override def pointer(location: Long): ImmediateValue[Long] with WordDoubleQuadSize = location
  }
}
