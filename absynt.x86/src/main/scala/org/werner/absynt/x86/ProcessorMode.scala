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
import org.werner.absynt.x86.instructions.branch.{Call, Jump, Loop}
import org.werner.absynt.x86.operands.Register.I8086Registers
import org.werner.absynt.x86.operands._
import org.werner.absynt.x86.operands.memoryaccess._
import org.werner.absynt.x86.operations.{AddressSizePrefixRequirement, OperandSizeInfo, OperandSizePrefixRequirement}
import scala.language.implicitConversions

sealed trait ArchitectureBounds {
  self: ProcessorMode =>
  type MaxValueSize <: ValueSize
  type MaxWideSize <: WordSize | DoubleWordSize | QuadWordSize

  given operandSizePrefixRequirement: OperandSizePrefixRequirement
  given addressSizePrefixRequirement: AddressSizePrefixRequirement

}

sealed abstract class ProcessorMode
extends ImmediateValue.I8086Implicits
  with MemoryAddress.I8086Implicits
  with FarPointer.I8086Implicits
{

  def pointer(location: Long): ImmediateValue[?] & (WordSize | DoubleWordSize | QuadWordSize)
  def shortPointer(location: Byte): NearPointer & ByteSize = ShortPointer(location)
  def wordPointer(location: Int): NearPointer & WordSize = LongPointer.realMode(location)
  def doubleWordPointer(location: Int): NearPointer & DoubleWordSize = LongPointer.protectedMode(location)
}

object ProcessorMode {
  trait LegacyBounds extends ArchitectureBounds {
    self: ProcessorMode =>
    type MaxValueSize = ByteSize | WordSize
    type MaxWideSize = WordSize
  }

  object Legacy extends ProcessorMode
    with OperandSizeInfo
    with LegacyBounds
    with Register.I8086Registers
    with IndirectMemoryLocation.LegacyOperations
    with Move.LegacyOperations
    with BasicInteraction.LegacyOperations
    with DivideMultiply.Operations
    with Interrupt.LegacyRealProtectedOperations
    with IO.LegacyOperations
    with Jump.LegacyOperations
    with Call.LegacyOperations
    with Stack.LegacyOperations
    with String.Operations
    with Test.LegacyOperations
    with Adjust.Operations
    with IncrementDecrement.LegacyOperations
    with Flags.Operations
    with Shift.Operations
    with Convert.LegacyOperations
    with Generic.LegacyOperations
    with Loop.Operations
  {
    given operandSizePrefixRequirement: OperandSizePrefixRequirement with {
      override def normalOperand(size: Operand & ValueSize): Boolean = false
      override def pointerOperand(size: Operand & FarPointerSize[?]): Boolean = false
    }

    given addressSizePrefixRequirement: AddressSizePrefixRequirement = _ => false

    override def pointer(location: Long): ImmediateValue[Short] & (WordSize | DoubleWordSize | QuadWordSize) = location.toShort
  }

  sealed trait I386Bounds extends ArchitectureBounds {
    self: ProcessorMode =>
    type MaxValueSize = ByteSize | WordSize | DoubleWordSize
    type MaxWideSize = WordSize | DoubleWordSize
  }

  object Real extends ProcessorMode
    with OperandSizeInfo
    with I386Bounds
    with Register.I386Registers
    with IndirectMemoryLocation.RealOperations
    with ImmediateValue.I386Implicits
    with MemoryAddress.I386Implicits
    with FarPointer.I386Implicits
    with Register.I386GenericRegisters
    with Move.RealOperations
    with BasicInteraction.I386Operations
    with DivideMultiply.Operations
    with Interrupt.LegacyRealProtectedOperations
    with IO.I386Operations
    with Jump.RealOperations
    with Call.RealOperations
    with Stack.I386Operations
    with String.Operations
    with Test.I386Operations
    with Adjust.Operations
    with IncrementDecrement.I386Operations
    with Flags.Operations
    with Shift.Operations
    with Exchange.Operations
    with Convert.I386Operations
    with Generic.I386Operations
    with Loop.Operations
  {

    given operandSizePrefixRequirement: OperandSizePrefixRequirement with {
      override def normalOperand(size: Operand & ValueSize): Boolean = size match {
        case _: DoubleWordSize => true
        case _ => false
      }
      override def pointerOperand(size: Operand & FarPointerSize[?]): Boolean = size match {
        case _: FarDoubleWordSize => true
        case _ => false
      }
    }

    given addressSizePrefixRequirement: AddressSizePrefixRequirement = _.isInstanceOf[DoubleWordSize]

    override def pointer(location: Long): ImmediateValue[Short] & (WordSize | DoubleWordSize | QuadWordSize) = location.toShort
  }

  object Protected extends ProcessorMode
    with OperandSizeInfo
    with I386Bounds
    with Register.I386Registers
    with IndirectMemoryLocation.ProtectedOperations
    with ImmediateValue.I386Implicits
    with MemoryAddress.I386Implicits
    with FarPointer.I386Implicits
    with Register.I386GenericRegisters
    with Move.ProtectedOperations
    with BasicInteraction.I386Operations
    with DivideMultiply.Operations
    with Interrupt.LegacyRealProtectedOperations
    with IO.I386Operations
    with Jump.ProtectedOperations
    with Call.ProtectedOperations
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
    with Generic.I386Operations
    with Loop.Operations
  {
    given operandSizePrefixRequirement: OperandSizePrefixRequirement with {
      override def normalOperand(size: Operand & ValueSize): Boolean = size match {
        case _: WordSize => true
        case _ => false
      }
      override def pointerOperand(size: Operand & FarPointerSize[?]): Boolean = size match {
        case _: FarWordSize => true
        case _ => false
      }
    }

    given addressSizePrefixRequirement: AddressSizePrefixRequirement = _.isInstanceOf[WordSize]

    override def pointer(location: Long): ImmediateValue[Int] & (WordSize | DoubleWordSize | QuadWordSize) = location.toInt
  }
  sealed trait LongBounds extends ArchitectureBounds {
    self: ProcessorMode =>

    type MaxValueSize = ValueSize
    type MaxWideSize = WordSize | DoubleWordSize | QuadWordSize
  }

  object Long extends ProcessorMode
    with OperandSizeInfo
    with LongBounds
    with Register.X64Registers
    with IndirectMemoryLocation.LongOperations
    with ImmediateValue.I386Implicits
    with ImmediateValue.X64Implicits
    with MemoryAddress.I386Implicits
    with MemoryAddress.X64Implicits
    with FarPointer.I386Implicits
    with Register.I386GenericRegisters
    with Register.X64GenericRegisters
    with Move.LongOperations
    with BasicInteraction.LongOperations
    with DivideMultiply.Operations
    with Interrupt.LongOperations
    with IO.LongOperations
    with Jump.LongOperations
    with Call.LongOperations
    with Stack.LongOperations
    with String.Operations
    with System.LongOperations
    with Test.LongOperations
    with IncrementDecrement.LongOperations
    with Flags.Operations
    with Shift.Operations
    with Exchange.Operations
    with Convert.LongOperations
    with Generic.I386Operations
    with Loop.Operations
  {
    given operandSizePrefixRequirement: OperandSizePrefixRequirement with {
      override def normalOperand(size: Operand & ValueSize): Boolean = size match {
        case _: WordSize => true
        case _ => false
      }
      override def pointerOperand(size: Operand & FarPointerSize[?]): Boolean = size match {
        case _: FarWordSize => true
        case _ => false
      }
    }

    given addressSizePrefixRequirement: AddressSizePrefixRequirement = _.isInstanceOf[DoubleWordSize]

    override def pointer(location: Long): ImmediateValue[Long] & (WordSize | DoubleWordSize | QuadWordSize) = location
  }
}
