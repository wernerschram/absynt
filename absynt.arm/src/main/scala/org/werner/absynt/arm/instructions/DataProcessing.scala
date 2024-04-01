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

package org.werner.absynt.arm.instructions

import org.werner.absynt._
import org.werner.absynt.arm.operands._
import org.werner.absynt.arm.operands.registers.GeneralRegister
import org.werner.absynt.arm.operations._
import org.werner.absynt.resource.{AbsoluteReference, RelativeReference, UnlabeledEncodable}

class DataProcessing private (val code: Byte, val opcode: String) {
  def apply(source1: GeneralRegister, source2: Shifter, destination: GeneralRegister, condition: Condition = Condition.Always): DataProcessingOperation =
    RegAndShifterToReg(source1, source2, destination, condition)

  private def RegAndShifterToReg(source1: GeneralRegister, source2: Shifter, destination: GeneralRegister,
                                 condition: Condition = Condition.Always) =
    new DataProcessingOperation(opcode, code, condition, source1, source2, destination)

  def setFlags(source1: GeneralRegister, source2: Shifter, destination: GeneralRegister, condition: Condition = Condition.Always): DataProcessingOperation & SetFlags =
    RegAndShifterToRegFlags(source1, source2, destination, condition)

  private def RegAndShifterToRegFlags(source1: GeneralRegister, source2: Shifter, destination: GeneralRegister,
                                      condition: Condition = Condition.Always) =
    new DataProcessingOperation(opcode, code, condition, source1, source2, destination) with SetFlags
}

class DataProcessingNoDestination(val code: Byte, val opcode: String) {
  def apply(register1: GeneralRegister, source2: Shifter, condition: Condition = Condition.Always): DataProcessingNoDestinationInstruction =
    RegAndShifter(register1, source2, condition)

  private def RegAndShifter(register1: GeneralRegister, operand2: Shifter, condition: Condition) =
    new DataProcessingNoDestinationInstruction(opcode, code, condition, register1, operand2)
}

class DataProcessingNoRegister(val code: Byte, val opcode: String) {
  def apply(source2: Shifter, destination: GeneralRegister, condition: Condition = Condition.Always): DataProcessingNoRegisterInstruction =
    ShifterToReg(source2, destination, condition)

  private def ShifterToReg(operand2: Shifter, destination: GeneralRegister, condition: Condition) = {
    new DataProcessingNoRegisterInstruction(opcode, code, condition, operand2, destination)
  }

  def setFlags(source2: Shifter, destination: GeneralRegister, condition: Condition = Condition.Always): DataProcessingNoRegisterInstruction & SetFlags =
    ShifterToRegFlags(source2, destination, condition)

  private def ShifterToRegFlags(operand2: Shifter, destination: GeneralRegister, condition: Condition) = {
    new DataProcessingNoRegisterInstruction(opcode, code, condition, operand2, destination) with SetFlags
  }
}

object DataProcessing {

  trait A32Operations {
    self: Shifter.A32Shifter =>

    object AddCarry extends DataProcessing(0x05.toByte, "adc") {
      def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Condition.Always): UnlabeledEncodable =
        if source2 == 0 then
          AddCarry(source1, 0, destination, condition)
        else {
          val shifters: Seq[RightRotateImmediate] = immediateShifter(source2)
          EncodableCollection(AddCarry(source1, shifters.head, destination, condition) +:
            shifters.tail.map(value => Add(destination, value, destination, condition)))
        }
    }

    object Add extends DataProcessing(0x04.toByte, "add") {

      def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Condition.Always): UnlabeledEncodable =
        if source2 == 0 then
          EncodableCollection(Nil)
        else {
          val shifters = immediateShifter(source2)
          EncodableCollection(Add(source1, shifters.head, destination, condition) +:
            shifters.tail.map(value => Add(destination, value, destination, condition)))
        }

      def forRelativeLabel(source1: GeneralRegister, targetLabel: Label, destination: GeneralRegister, encodableCondition: Condition = Condition.Always): RelativeReference =
        new RelativeReference() with NamedConditional {
          override val target: Label = targetLabel

          override def sizeForDependencySize(distance: Int, offsetDirection: OffsetDirection): Int =
            encodableForDependencySize(distance, offsetDirection).size

          override def possibleSizes: Set[Int] = Set(4, 8, 12, 16)

          override def toString = s"$mnemonicString $destination, $source1, $target"

          override def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): UnlabeledEncodable =
            forConstant(source1, ArmRelativeOffset.positionalOffset(distance)(offsetDirection).offset, destination, condition)

          override val condition: Condition = encodableCondition

          override val opcode: String = "add"
        }
    }

    object And extends DataProcessing(0x00.toByte, "and") {
      def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Condition.Always): UnlabeledEncodable =
        if source2 == 0 then
          And(source1, 0, destination, condition)
        else {
          val shifters: Seq[RightRotateImmediate] = immediateShifter(~source2)
          EncodableCollection(BitClear(source1, shifters.head, destination, condition) +:
            shifters.tail.map(value => BitClear(destination, value, destination, condition)))
        }
    }

    object BitClear extends DataProcessing(0x0E.toByte, "bic") {
      def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Condition.Always): UnlabeledEncodable =
        if source2 == 0 then
          EncodableCollection(Nil)
        else {
          val shifters: Seq[RightRotateImmediate] = immediateShifter(source2)
          EncodableCollection(BitClear(source1, shifters.head, destination, condition) +:
            shifters.tail.map(value => BitClear(destination, value, destination, condition)))
        }
    }

    object CompareNegative extends DataProcessingNoDestination(0x0B.toByte, "cmn")

    object Compare extends DataProcessingNoDestination(0x0A.toByte, "cmp")

    object ExclusiveOr extends DataProcessing(0x01.toByte, "eor") {
      def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Condition.Always): UnlabeledEncodable =
        if source2 == 0 then
          EncodableCollection(Nil)
        else {
          val shifters: Seq[RightRotateImmediate] = immediateShifter(source2)
          EncodableCollection(ExclusiveOr(source1, shifters.head, destination, condition) +:
            shifters.tail.map(value => ExclusiveOr(destination, value, destination, condition)))
        }
    }

    object Move extends DataProcessingNoRegister(0x0D.toByte, "mov") {
      def forConstant(source2: Int, destination: GeneralRegister, condition: Condition = Condition.Always): UnlabeledEncodable =
        if source2 == 0 then
          Move(0, destination, condition)
        else {
          val shifters: Seq[RightRotateImmediate] = immediateShifter(source2)
          EncodableCollection(Move(shifters.head, destination, condition) +:
            shifters.tail.map(value => Or(destination, value, destination, condition)))
        }

      def forLabel(targetLabel: Label, destination: GeneralRegister, internalCondition: Condition = Condition.Always): AbsoluteReference =
        new AbsoluteReference(targetLabel) with NamedConditional {
          override def sizeForDistance(distance: Int): Int = encodableForDistance(distance).size

          override def encodableForDistance(distance: Int): UnlabeledEncodable = forConstant(distance, destination, condition)

          override def possibleSizes: Set[Int] = Set(4, 8, 12, 16)

          override def toString = s"$mnemonicString $destination, $target"

          override val condition: Condition = internalCondition

          override val opcode: String = "mov"
        }
    }

    object MoveNot extends DataProcessingNoRegister(0x0F.toByte, "mvn")

    object Or extends DataProcessing(0x0C.toByte, "orr") {
      def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Condition.Always): UnlabeledEncodable =
        if source2 == 0 then
          EncodableCollection(Nil)
        else {
          val shifters: Seq[RightRotateImmediate] = immediateShifter(source2)
          EncodableCollection(Or(source1, shifters.head, destination, condition) +:
            shifters.tail.map(value => Or(destination, value, destination, condition)))
        }
    }

    object ReverseSubtract extends DataProcessing(0x03.toByte, "rsb") {
      def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Condition.Always): UnlabeledEncodable =
        if source2 == 0 then
          ReverseSubtract(source1, 0, destination, condition)
        else {
          val shifters: Seq[RightRotateImmediate] = immediateShifter(source2)
          EncodableCollection(ReverseSubtract(source1, shifters.head, destination, condition) +:
            shifters.tail.map(value => Add(destination, value, destination, condition)))
        }
    }

    object ReverseSubtractCarry extends DataProcessing(0x07.toByte, "rsc") {
      def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Condition.Always): UnlabeledEncodable =
        if source2 == 0 then
          ReverseSubtractCarry(source1, 0, destination, condition)
        else {
          val shifters: Seq[RightRotateImmediate] = immediateShifter(source2)
          EncodableCollection(ReverseSubtractCarry(source1, shifters.head, destination, condition) +:
            shifters.tail.map(value => Add(destination, value, destination, condition)))
        }
    }

    object SubtractCarry extends DataProcessing(0x06.toByte, "sbc") {
      def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Condition.Always): UnlabeledEncodable =
        if source2 == 0 then
          SubtractCarry(source1, 0, destination, condition)
        else {
          val shifters: Seq[RightRotateImmediate] = immediateShifter(source2)
          EncodableCollection(SubtractCarry(source1, shifters.head, destination, condition) +:
            shifters.tail.map(value => Subtract(destination, value, destination, condition)))
        }
    }

    object Subtract extends DataProcessing(0x02.toByte, "sub") {
      def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Condition.Always): UnlabeledEncodable =
        if source2 == 0 then
          EncodableCollection(Nil)
        else {
          val shifters: Seq[RightRotateImmediate] = immediateShifter(source2)
          EncodableCollection(Subtract(source1, shifters.head, destination, condition) +:
            shifters.tail.map(value => Subtract(destination, value, destination, condition)))
        }
    }

    object TestEquivalence extends DataProcessingNoDestination(0x09.toByte, "teq")

    object Test extends DataProcessingNoDestination(0x08.toByte, "tst")

  }

}
