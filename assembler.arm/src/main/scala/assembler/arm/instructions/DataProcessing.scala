package assembler.arm.instructions

import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition._
import assembler.arm.operands.registers.GeneralRegister
import assembler.arm.operands.{RightRotateImmediate, Shifter}
import assembler.arm.operations._
import assembler.sections.Section
import assembler.{Encodable, EncodableCollection, Label}

class DataProcessing(val code: Byte, val opcode: String) {
  def apply(source1: GeneralRegister, source2: Shifter, destination: GeneralRegister, condition: Condition = Always)
           (implicit processorMode: ProcessorMode, label: Label) =
    RegAndShifterToReg(label, source1, source2, destination, condition)

  private def RegAndShifterToReg(label: Label, source1: GeneralRegister, source2: Shifter, destination: GeneralRegister,
                                 condition: Condition = Always) =
    new DataProcessingOperation(label, opcode, code, condition, source1, source2, destination)

  def setFlags(source1: GeneralRegister, source2: Shifter, destination: GeneralRegister, condition: Condition = Always)
              (implicit processorMode: ProcessorMode, label: Label) =
    RegAndShifterToRegFlags(label, source1, source2, destination, condition)

  private def RegAndShifterToRegFlags(label: Label, source1: GeneralRegister, source2: Shifter, destination: GeneralRegister,
                                      condition: Condition = Always) =
    new DataProcessingOperation(label, opcode, code, condition, source1, source2, destination) with SetFlags
}

class DataProcessingNoDestination(val code: Byte, val opcode: String) {
  def apply(register1: GeneralRegister, source2: Shifter, condition: Condition = Always)
           (implicit processorMode: ProcessorMode, label: Label) =
    RegAndShifter(label, register1, source2, condition)

  private def RegAndShifter(label: Label, register1: GeneralRegister, operand2: Shifter, condition: Condition) =
    new DataProcessingNoDestinationInstruction(label, opcode, code, condition, register1, operand2)
}

class DataProcessingNoRegister(val code: Byte, val opcode: String) {
  def apply(source2: Shifter, destination: GeneralRegister, condition: Condition = Always)
           (implicit processorMode: ProcessorMode, label: Label) =
    ShifterToReg(label, source2, destination, condition)

  private def ShifterToReg(label: Label, operand2: Shifter, destination: GeneralRegister, condition: Condition) = {
    new DataProcessingNoRegisterInstruction(label, opcode, code, condition, operand2, destination)
  }

  def setFlags(source2: Shifter, destination: GeneralRegister, condition: Condition = Always)
              (implicit processorMode: ProcessorMode, label: Label) =
    ShifterToRegFlags(label, source2, destination, condition)

  private def ShifterToRegFlags(label: Label, operand2: Shifter, destination: GeneralRegister, condition: Condition) = {
    new DataProcessingNoRegisterInstruction(label, opcode, code, condition, operand2, destination) with SetFlags
  }
}

object AddCarry extends DataProcessing(0x05.toByte, "adc") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always)
                 (implicit processorMode: ProcessorMode, label: Label): Encodable = {
    if (source2 == 0)
      return EncodableCollection(apply(source1, Shifter.RightRotateImmediate(0, 0), destination, condition) :: Nil)
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    EncodableCollection(apply(source1, shifters.head, destination, condition) ::
      shifters.tail.map(value => Add(destination, value, destination, condition)))
  }
}

object Add extends DataProcessing(0x04.toByte, "add") {
  def forShifters(source1: GeneralRegister, shifters: List[RightRotateImmediate], destination: GeneralRegister,
                  condition: Condition = Always)(implicit processorMode: ProcessorMode, label: Label): Encodable = {
    if (shifters.isEmpty) {
      return if (label == Label.noLabel)
        EncodableCollection(Nil)
      else
        EncodableCollection(apply(source1, Shifter.RightRotateImmediate(0, 0), destination, condition) :: Nil)
    }
    EncodableCollection(apply(source1, shifters.head, destination, condition) ::
      shifters.tail.map(value => Add(destination, value, destination, condition)))
  }

  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always)
                 (implicit processorMode: ProcessorMode, label: Label): Encodable =
    forShifters(source1, Shifter.apply(source2), destination, condition)
}

object And extends DataProcessing(0x00.toByte, "and") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always)
                 (implicit processorMode: ProcessorMode, label: Label): Encodable = {
    if (source2 == 0)
      return EncodableCollection(apply(source1, Shifter.RightRotateImmediate(0, 0), destination, condition) :: Nil)
    val shifters: List[RightRotateImmediate] = Shifter.apply(~source2)
    EncodableCollection(BitClear(source1, shifters.head, destination, condition) ::
      shifters.tail.map(value => BitClear(destination, value, destination, condition)))
  }
}

object BitClear extends DataProcessing(0x0E.toByte, "bic") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always)
                 (implicit processorMode: ProcessorMode, label: Label): Encodable = {
    if (source2 == 0) {
      return if (label == Label.noLabel)
        EncodableCollection(Nil)
      else
        EncodableCollection(apply(source1, Shifter.RightRotateImmediate(0, 0), destination, condition) :: Nil)
    }
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    EncodableCollection(BitClear(source1, shifters.head, destination, condition) ::
      shifters.tail.map(value => BitClear(destination, value, destination, condition)))
  }
}

object CompareNegative extends DataProcessingNoDestination(0x0B.toByte, "cmn")

object Compare extends DataProcessingNoDestination(0x0A.toByte, "cmp")

object ExclusiveOr extends DataProcessing(0x01.toByte, "eor") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always)
                 (implicit processorMode: ProcessorMode, label: Label): Encodable = {
    if (source2 == 0) {
      return if (label == Label.noLabel)
        EncodableCollection(Nil)
      else
        EncodableCollection(apply(source1, Shifter.RightRotateImmediate(0, 0), destination, condition) :: Nil)
    }
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    EncodableCollection(ExclusiveOr(source1, shifters.head, destination, condition) ::
      shifters.tail.map(value => ExclusiveOr(destination, value, destination, condition)))
  }
}

object Move extends DataProcessingNoRegister(0x0D.toByte, "mov") {
  def forConstant(source2: Int, destination: GeneralRegister, condition: Condition = Always)
                 (implicit processorMode: ProcessorMode, label: Label): Encodable = {
    if (source2 == 0)
      return EncodableCollection(apply(Shifter.RightRotateImmediate(0, 0), destination, condition) :: Nil)
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    EncodableCollection(apply(shifters.head, destination, condition) ::
      shifters.tail.map(value => Or(destination, value, destination, condition)))
  }

  def forLabel(targetLabel: Label, destination: GeneralRegister, condition: Condition = Always)
              (implicit processorMode: ProcessorMode, label: Label): Encodable =
    new ReferencingARMOperation(label, opcode, targetLabel, Always) {
      override def encodableForDistance(distance: Int)(implicit page: Section): Encodable =
        forConstant(distance + page.getRelativeAddress(this), destination, condition)
    }
}

object MoveNot extends DataProcessingNoRegister(0x0F.toByte, "mvn")

object Or extends DataProcessing(0x0C.toByte, "orr") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always)
                 (implicit processorMode: ProcessorMode, label: Label): Encodable = {
    if (source2 == 0) {
      return if (label == Label.noLabel)
        EncodableCollection(Nil)
      else
        EncodableCollection(apply(source1, Shifter.RightRotateImmediate(0, 0), destination, condition) :: Nil)
    }
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    EncodableCollection(apply(source1, shifters.head, destination, condition) ::
      shifters.tail.map(value => Or(destination, value, destination, condition)))
  }
}

object ReverseSubtract extends DataProcessing(0x03.toByte, "rsb") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always)
                 (implicit processorMode: ProcessorMode, label: Label): Encodable = {
    if (source2 == 0)
      return EncodableCollection(ReverseSubtract(source1, Shifter.RightRotateImmediate(0, 0), destination, condition) :: Nil)
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    EncodableCollection(ReverseSubtract(source1, shifters.head, destination, condition) ::
      shifters.tail.map(value => Add(destination, value, destination, condition)))
  }
}

object ReverseSubtractCarry extends DataProcessing(0x07.toByte, "rsc") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always)
                 (implicit processorMode: ProcessorMode, label: Label): Encodable = {
    if (source2 == 0)
      return EncodableCollection(ReverseSubtractCarry(source1, Shifter.RightRotateImmediate(0, 0), destination, condition) :: Nil)
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    EncodableCollection(ReverseSubtractCarry(source1, shifters.head, destination, condition) ::
      shifters.tail.map(value => Add(destination, value, destination, condition)))
  }
}

object SubtractCarry extends DataProcessing(0x06.toByte, "sbc") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always)
                 (implicit processorMode: ProcessorMode, label: Label): Encodable = {
    if (source2 == 0)
      return EncodableCollection(apply(source1, Shifter.RightRotateImmediate(0, 0), destination, condition) :: Nil)
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    EncodableCollection(SubtractCarry(source1, shifters.head, destination, condition) ::
      shifters.tail.map(value => Subtract(destination, value, destination, condition)))
  }
}

object Subtract extends DataProcessing(0x02.toByte, "sub") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always)
                 (implicit processorMode: ProcessorMode, label: Label): Encodable = {
    if (source2 == 0) {
      return if (label == Label.noLabel)
        EncodableCollection(Nil)
      else
        EncodableCollection(apply(source1, Shifter.RightRotateImmediate(0, 0), destination, condition) :: Nil)
    }
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    EncodableCollection(Subtract(source1, shifters.head, destination, condition) ::
      shifters.tail.map(value => Subtract(destination, value, destination, condition)))
  }
}

object TestEquivalence extends DataProcessingNoDestination(0x09.toByte, "teq")

object Test extends DataProcessingNoDestination(0x08.toByte, "tst")
