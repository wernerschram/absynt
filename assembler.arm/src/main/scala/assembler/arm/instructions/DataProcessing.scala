package assembler.arm.instructions

import assembler._
import assembler.arm.operands.Condition._
import assembler.arm.operands._
import assembler.arm.operands.registers.GeneralRegister
import assembler.arm.operations._
import assembler.resource.{AbsoluteReference, Encodable, RelativeReference, UnlabeledEncodable}

class DataProcessing(val code: Byte, val opcode: String) {
  def apply(source1: GeneralRegister, source2: Shifter, destination: GeneralRegister, condition: Condition = Always): DataProcessingOperation =
    RegAndShifterToReg(source1, source2, destination, condition)

  private def RegAndShifterToReg(source1: GeneralRegister, source2: Shifter, destination: GeneralRegister,
                                 condition: Condition = Always) =
    new DataProcessingOperation(opcode, code, condition, source1, source2, destination)

  def setFlags(source1: GeneralRegister, source2: Shifter, destination: GeneralRegister, condition: Condition = Always): DataProcessingOperation with SetFlags =
    RegAndShifterToRegFlags(source1, source2, destination, condition)

  private def RegAndShifterToRegFlags(source1: GeneralRegister, source2: Shifter, destination: GeneralRegister,
                                      condition: Condition = Always) =
    new DataProcessingOperation(opcode, code, condition, source1, source2, destination) with SetFlags
}

class DataProcessingNoDestination(val code: Byte, val opcode: String) {
  def apply(register1: GeneralRegister, source2: Shifter, condition: Condition = Always): DataProcessingNoDestinationInstruction =
    RegAndShifter(register1, source2, condition)

  private def RegAndShifter(register1: GeneralRegister, operand2: Shifter, condition: Condition) =
    new DataProcessingNoDestinationInstruction(opcode, code, condition, register1, operand2)
}

class DataProcessingNoRegister(val code: Byte, val opcode: String) {
  def apply(source2: Shifter, destination: GeneralRegister, condition: Condition = Always): DataProcessingNoRegisterInstruction =
    ShifterToReg(source2, destination, condition)

  private def ShifterToReg(operand2: Shifter, destination: GeneralRegister, condition: Condition) = {
    new DataProcessingNoRegisterInstruction(opcode, code, condition, operand2, destination)
  }

  def setFlags(source2: Shifter, destination: GeneralRegister, condition: Condition = Always): DataProcessingNoRegisterInstruction with SetFlags =
    ShifterToRegFlags(source2, destination, condition)

  private def ShifterToRegFlags(operand2: Shifter, destination: GeneralRegister, condition: Condition) = {
    new DataProcessingNoRegisterInstruction(opcode, code, condition, operand2, destination) with SetFlags
  }
}

object AddCarry extends DataProcessing(0x05.toByte, "adc") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always): UnlabeledEncodable =
    if (source2 == 0)
      AddCarry(source1, 0, destination, condition)
    else {
      val shifters: Seq[RightRotateImmediate] = Shifter.apply(source2)
      EncodableCollection(apply(source1, shifters.head, destination, condition) +:
        shifters.tail.map(value => Add(destination, value, destination, condition)))
    }
}

object Add extends DataProcessing(0x04.toByte, "add") {

  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always): UnlabeledEncodable =
    if (source2 == 0)
      EncodableCollection(Nil)
    else {
      val shifters = Shifter.apply(source2)
      EncodableCollection(apply(source1, shifters.head, destination, condition) +:
        shifters.tail.map(value => Add(destination, value, destination, condition)))
    }

  def forRelativeLabel(source1: GeneralRegister, targetLabel: Label, destination: GeneralRegister, condition: Condition = Always): RelativeReference =
    new RelativeReference(targetLabel) with NamedConditional {
      override def sizeForDependencySize(distance: Int, offsetDirection: OffsetDirection): Int =
        encodableForDependencySize(distance, offsetDirection).size

      override def possibleSizes: Set[Int] = Set(4, 8, 12, 16)

      override def toString = s"$mnemonicString $destination, $source1, $target"
          override def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): UnlabeledEncodable =
            forConstant(source1, ArmRelativeOffset.positionalOffset(distance)(offsetDirection).offset, destination, condition)

      override val condition: Condition = Always

      override val opcode: String = "add"
    }
}

object And extends DataProcessing(0x00.toByte, "and") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always): UnlabeledEncodable =
    if (source2 == 0)
      And(source1, 0, destination, condition)
    else {
      val shifters: Seq[RightRotateImmediate] = Shifter.apply(~source2)
      EncodableCollection(BitClear(source1, shifters.head, destination, condition) +:
        shifters.tail.map(value => BitClear(destination, value, destination, condition)))
    }
}

object BitClear extends DataProcessing(0x0E.toByte, "bic") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always): UnlabeledEncodable =
    if (source2 == 0)
        EncodableCollection(Nil)
    else {
      val shifters: Seq[RightRotateImmediate] = Shifter.apply(source2)
      EncodableCollection(BitClear(source1, shifters.head, destination, condition) +:
        shifters.tail.map(value => BitClear(destination, value, destination, condition)))
    }
}

object CompareNegative extends DataProcessingNoDestination(0x0B.toByte, "cmn")

object Compare extends DataProcessingNoDestination(0x0A.toByte, "cmp")

object ExclusiveOr extends DataProcessing(0x01.toByte, "eor") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always): UnlabeledEncodable =
    if (source2 == 0)
        EncodableCollection(Nil)
    else {
      val shifters: Seq[RightRotateImmediate] = Shifter.apply(source2)
      EncodableCollection(ExclusiveOr(source1, shifters.head, destination, condition) +:
        shifters.tail.map(value => ExclusiveOr(destination, value, destination, condition)))
    }
}

object Move extends DataProcessingNoRegister(0x0D.toByte, "mov") {
  def forConstant(source2: Int, destination: GeneralRegister, condition: Condition = Always): UnlabeledEncodable =
    if (source2 == 0)
      Move(0, destination, condition)
    else {
      val shifters: Seq[RightRotateImmediate] = Shifter.apply(source2)
      EncodableCollection(apply(shifters.head, destination, condition) +:
        shifters.tail.map(value => Or(destination, value, destination, condition)))
    }

  def forLabel(targetLabel: Label, destination: GeneralRegister, condition: Condition = Always): AbsoluteReference =
    new AbsoluteReference(targetLabel) {
      override def sizeForDistance(distance: Int): Int = encodableForDistance(distance).size

      override def encodableForDistance(distance: Int): UnlabeledEncodable = forConstant(distance, destination, condition)

      override def possibleSizes: Set[Int] = Set(4, 8, 12, 16)
    }
}

object MoveNot extends DataProcessingNoRegister(0x0F.toByte, "mvn")

object Or extends DataProcessing(0x0C.toByte, "orr") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always): UnlabeledEncodable =
    if (source2 == 0)
        EncodableCollection(Nil)
    else {
      val shifters: Seq[RightRotateImmediate] = Shifter.apply(source2)
      EncodableCollection(apply(source1, shifters.head, destination, condition) +:
        shifters.tail.map(value => Or(destination, value, destination, condition)))
    }
}

object ReverseSubtract extends DataProcessing(0x03.toByte, "rsb") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always): UnlabeledEncodable =
    if (source2 == 0)
      ReverseSubtract(source1, 0, destination, condition)
    else {
      val shifters: Seq[RightRotateImmediate] = Shifter.apply(source2)
      EncodableCollection(ReverseSubtract(source1, shifters.head, destination, condition) +:
        shifters.tail.map(value => Add(destination, value, destination, condition)))
    }
}

object ReverseSubtractCarry extends DataProcessing(0x07.toByte, "rsc") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always): UnlabeledEncodable =
    if (source2 == 0)
      ReverseSubtractCarry(source1, 0, destination, condition)
    else {
      val shifters: Seq[RightRotateImmediate] = Shifter.apply(source2)
      EncodableCollection(ReverseSubtractCarry(source1, shifters.head, destination, condition) +:
        shifters.tail.map(value => Add(destination, value, destination, condition)))
    }
}

object SubtractCarry extends DataProcessing(0x06.toByte, "sbc") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always): UnlabeledEncodable =
    if (source2 == 0)
      SubtractCarry(source1, 0, destination, condition)
    else {
      val shifters: Seq[RightRotateImmediate] = Shifter.apply(source2)
      EncodableCollection(SubtractCarry(source1, shifters.head, destination, condition) +:
        shifters.tail.map(value => Subtract(destination, value, destination, condition)))
    }
}

object Subtract extends DataProcessing(0x02.toByte, "sub") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always): UnlabeledEncodable =
    if (source2 == 0)
      EncodableCollection(Nil)
    else {
      val shifters: Seq[RightRotateImmediate] = Shifter.apply(source2)
      EncodableCollection(Subtract(source1, shifters.head, destination, condition) +:
        shifters.tail.map(value => Subtract(destination, value, destination, condition)))
    }
}

object TestEquivalence extends DataProcessingNoDestination(0x09.toByte, "teq")

object Test extends DataProcessingNoDestination(0x08.toByte, "tst")
