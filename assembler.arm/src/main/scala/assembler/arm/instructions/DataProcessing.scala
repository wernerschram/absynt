package assembler.arm.instructions

import assembler.{Designation, Encodable, Unlabeled, Label}
import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition._
import assembler.arm.operands.registers.GeneralRegister
import assembler.arm.operands.{RightRotateImmediate, Shifter}
import assembler.arm.operations._
import assembler.arm.instructions.DataProcessing._

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

object DataProcessing {
  type Designator = (Encodable)=>Designation[Encodable]
  val UnLabeledDesignator: Designator = Unlabeled(_)
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
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always,
                  designator: Designator = UnLabeledDesignator)
                 (implicit processorMode: ProcessorMode, label: Label): List[Designation[Encodable]] = {
    if (source2 == 0)
      return designator(apply(source1, Shifter.RightRotateImmediate(0, 0), destination, condition)) :: Nil
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    designator(apply(source1, shifters.head, destination, condition)) ::
      shifters.tail.map(value => Unlabeled(Add(destination, value, destination, condition)))
  }
}

object Add extends DataProcessing(0x04.toByte, "add") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always,
                  designator: Designator = UnLabeledDesignator)
                 (implicit processorMode: ProcessorMode, label: Label): List[Designation[Encodable]] = {
    if (source2 == 0) {
      return if (designator == UnLabeledDesignator)
        Nil
      else
        designator(apply(source1, Shifter.RightRotateImmediate(0, 0), destination, condition)) :: Nil
    }
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    designator(apply(source1, shifters.head, destination, condition)) ::
      shifters.tail.map(value => Unlabeled(Add(destination, value, destination, condition)))
  }
}

object And extends DataProcessing(0x00.toByte, "and") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always,
                  designator: Designator = UnLabeledDesignator)
                 (implicit processorMode: ProcessorMode, label: Label): List[Designation[Encodable]] = {
    if (source2 == 0)
      return designator(apply(source1, Shifter.RightRotateImmediate(0, 0), destination, condition)) :: Nil
    val shifters: List[RightRotateImmediate] = Shifter.apply(~source2)
    designator(BitClear(source1, shifters.head, destination, condition)) ::
      shifters.tail.map(value => Unlabeled(BitClear(destination, value, destination, condition)))
  }
}

object BitClear extends DataProcessing(0x0E.toByte, "bic") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always,
                  designator: Designator = UnLabeledDesignator)
                 (implicit processorMode: ProcessorMode, label: Label): List[Designation[Encodable]] = {
    if (source2 == 0) {
      return if (designator == UnLabeledDesignator)
        Nil
      else
        designator(apply(source1, Shifter.RightRotateImmediate(0, 0), destination, condition)) :: Nil
    }
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    designator(BitClear(source1, shifters.head, destination, condition)) ::
      shifters.tail.map(value => Unlabeled(BitClear(destination, value, destination, condition)))
  }
}

object CompareNegative extends DataProcessingNoDestination(0x0B.toByte, "cmn")

object Compare extends DataProcessingNoDestination(0x0A.toByte, "cmp")

object ExclusiveOr extends DataProcessing(0x01.toByte, "eor") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always,
                  designator: Designator = UnLabeledDesignator)
                 (implicit processorMode: ProcessorMode, label: Label): List[Designation[Encodable]] = {
    if (source2 == 0) {
      return if (designator == UnLabeledDesignator)
        Nil
      else
        designator(apply(source1, Shifter.RightRotateImmediate(0, 0), destination, condition)) :: Nil
    }
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    designator(ExclusiveOr(source1, shifters.head, destination, condition)) ::
      shifters.tail.map(value => Unlabeled(ExclusiveOr(destination, value, destination, condition)))
  }
}

object Move extends DataProcessingNoRegister(0x0D.toByte, "mov") {
  def forConstant(source2: Int, destination: GeneralRegister, condition: Condition = Always,
                  designator: Designator = UnLabeledDesignator)
                 (implicit processorMode: ProcessorMode, label: Label): List[Designation[Encodable]] = {
    if (source2 == 0)
      return designator(apply(Shifter.RightRotateImmediate(0, 0), destination, condition)) :: Nil
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    designator(apply(shifters.head, destination, condition)) ::
      shifters.tail.map(value => Unlabeled(Or(destination, value, destination, condition)))
  }
}

object MoveNot extends DataProcessingNoRegister(0x0F.toByte, "mvn")

object Or extends DataProcessing(0x0C.toByte, "orr") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always,
                  designator: Designator = UnLabeledDesignator)
                 (implicit processorMode: ProcessorMode, label: Label): List[Designation[Encodable]] = {
    if (source2 == 0) {
      return if (designator == UnLabeledDesignator)
        Nil
      else
        designator(apply(source1, Shifter.RightRotateImmediate(0, 0), destination, condition)) :: Nil
    }
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    designator(apply(source1, shifters.head, destination, condition)) ::
      shifters.tail.map(value => Unlabeled(Or(destination, value, destination, condition)))
  }
}

object ReverseSubtract extends DataProcessing(0x03.toByte, "rsb") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always,
                  designator: Designator = UnLabeledDesignator)
                 (implicit processorMode: ProcessorMode, label: Label): List[Designation[Encodable]] = {
    if (source2 == 0)
      return designator(ReverseSubtract(source1, Shifter.RightRotateImmediate(0, 0), destination, condition)) :: Nil
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    designator(ReverseSubtract(source1, shifters.head, destination, condition)) ::
      shifters.tail.map(value => Unlabeled(Add(destination, value, destination, condition)))
  }
}

object ReverseSubtractCarry extends DataProcessing(0x07.toByte, "rsc") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always,
                  designator: Designator = UnLabeledDesignator)
                 (implicit processorMode: ProcessorMode, label: Label): List[Designation[Encodable]] = {
    if (source2 == 0)
      return designator(ReverseSubtractCarry(source1, Shifter.RightRotateImmediate(0, 0), destination, condition)) :: Nil
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    designator(ReverseSubtractCarry(source1, shifters.head, destination, condition)) ::
      shifters.tail.map(value => Unlabeled(Add(destination, value, destination, condition)))
  }
}

object SubtractCarry extends DataProcessing(0x06.toByte, "sbc") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always,
                  designator: Designator = UnLabeledDesignator)
                 (implicit processorMode: ProcessorMode, label: Label): List[Designation[Encodable]] = {
    if (source2 == 0)
      return designator(apply(source1, Shifter.RightRotateImmediate(0, 0), destination, condition)) :: Nil
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    designator(SubtractCarry(source1, shifters.head, destination, condition)) ::
      shifters.tail.map(value => Unlabeled(Subtract(destination, value, destination, condition)))
  }
}

object Subtract extends DataProcessing(0x02.toByte, "sub") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always,
                  designator: Designator = UnLabeledDesignator)
                 (implicit processorMode: ProcessorMode, label: Label): List[Designation[Encodable]] = {
    if (source2 == 0) {
      return if (designator == UnLabeledDesignator)
        Nil
      else
        designator(apply(source1, Shifter.RightRotateImmediate(0, 0), destination, condition)) :: Nil
    }
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    designator(Subtract(source1, shifters.head, destination, condition)) ::
      shifters.tail.map(value => Unlabeled(Subtract(destination, value, destination, condition)))
  }
}

object TestEquivalence extends DataProcessingNoDestination(0x09.toByte, "teq")

object Test extends DataProcessingNoDestination(0x08.toByte, "tst")
