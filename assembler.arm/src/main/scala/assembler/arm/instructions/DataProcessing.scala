package assembler.arm.instructions

import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition._
import assembler.arm.operands.RightRotateImmediate
import assembler.arm.operands.Shifter
import assembler.arm.operands.registers.GeneralRegister
import assembler.arm.operations.ARMOperation
import assembler.arm.operations.DataProcessingOperation
import assembler.arm.operations.SetFlags
import assembler.arm.operations.DataProcessingNoDestinationInstruction
import assembler.arm.operations.DataProcessingNoRegisterInstruction

class DataProcessing(val code: Byte, val opcode: String) {
  private def RegAndShifterToReg(source1: GeneralRegister, source2: Shifter, destination: GeneralRegister, condition: Condition = Always) =
    new DataProcessingOperation(opcode, code, condition, source1, source2, destination)

  private def RegAndShifterToRegFlags(source1: GeneralRegister, source2: Shifter, destination: GeneralRegister, condition: Condition = Always) =
    new DataProcessingOperation(opcode, code, condition, source1, source2, destination) with SetFlags

  def apply(source1: GeneralRegister, source2: Shifter, destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    RegAndShifterToReg(source1, source2, destination, condition)

  def setFlags(source1: GeneralRegister, source2: Shifter, destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    RegAndShifterToRegFlags(source1, source2, destination, condition)
}

class DataProcessingNoDestination(val code: Byte, val opcode: String) {
  private def RegAndShifter(register1: GeneralRegister, operand2: Shifter, condition: Condition) =
    new DataProcessingNoDestinationInstruction(opcode, code, condition, register1, operand2)

  def apply(register1: GeneralRegister, source2: Shifter, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    RegAndShifter(register1, source2, condition)
}

class DataProcessingNoRegister(val code: Byte, val opcode: String) {
//  private val ShifterToReg = new DataProcessingOpcode(code)(opcode)
  private def ShifterToReg(operand2: Shifter, destination: GeneralRegister, condition: Condition) = {
    new DataProcessingNoRegisterInstruction(opcode, code, condition, operand2, destination)
  }

  private def ShifterToRegFlags(operand2: Shifter, destination: GeneralRegister, condition: Condition) = {
    new DataProcessingNoRegisterInstruction(opcode, code, condition, operand2, destination) with SetFlags
  }

  def apply(source2: Shifter, destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    ShifterToReg(source2, destination, condition)

  def setFlags(source2: Shifter, destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    ShifterToRegFlags(source2, destination, condition)
}

object AddCarry extends DataProcessing(0x05.toByte, "adc") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode): List[ARMOperation] = {
    if (source2 == 0)
      return apply(source1, Shifter.RightRotateImmediate(0, 0), destination, condition) :: Nil
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    apply(source1, shifters.head, destination, condition) ::
      shifters.tail.map(value => Add(destination, value, destination, condition))
  }
}
object Add extends DataProcessing(0x04.toByte, "add") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode): List[ARMOperation] = {
    if (source2 == 0)
      return Nil
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    apply(source1, shifters.head, destination, condition) ::
      shifters.tail.map(value => Add(destination, value, destination, condition))
  }
}
object And extends DataProcessing(0x00.toByte, "and") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode): List[ARMOperation] = {
    if (source2 == 0)
      return apply(source1, Shifter.RightRotateImmediate(0, 0), destination, condition) :: Nil
    val shifters: List[RightRotateImmediate] = Shifter.apply(~source2)
    BitClear(source1, shifters.head, destination, condition) ::
      shifters.tail.map(value => BitClear(destination, value, destination, condition))
  }
}
object BitClear extends DataProcessing(0x0E.toByte, "bic") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode): List[ARMOperation] = {
    if (source2 == 0)
      return Nil
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    BitClear(source1, shifters.head, destination, condition) ::
      shifters.tail.map(value => BitClear(destination, value, destination, condition))
  }
}
object CompareNegative extends DataProcessingNoDestination(0x0B.toByte, "cmn")
object Compare extends DataProcessingNoDestination(0x0A.toByte, "cmp")
object ExclusiveOr extends DataProcessing(0x01.toByte, "eor") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode): List[ARMOperation] = {
    if (source2 == 0)
      return Nil
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    ExclusiveOr(source1, shifters.head, destination, condition) ::
      shifters.tail.map(value => ExclusiveOr(destination, value, destination, condition))
  }
}
object Move extends DataProcessingNoRegister(0x0D.toByte, "mov") {
  def forConstant(source2: Int, destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode): List[ARMOperation] = {
    if (source2 == 0)
      return apply(Shifter.RightRotateImmediate(0, 0), destination, condition) :: Nil
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
      apply(shifters.head, destination, condition) :: shifters.tail.map(value => Or(destination, value, destination, condition))
  }
}
object MoveNot extends DataProcessingNoRegister(0x0F.toByte, "mvn")
object Or extends DataProcessing(0x0C.toByte, "orr") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode): List[ARMOperation] = {
    if (source2 == 0)
      return Nil
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    apply(source1, shifters.head, destination, condition) ::
      shifters.tail.map(value => Or(destination, value, destination, condition))
  }
}
object ReverseSubtract extends DataProcessing(0x03.toByte, "rsb") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode): List[ARMOperation] = {
    if (source2 == 0)
      return ReverseSubtract(source1, Shifter.RightRotateImmediate(0, 0), destination, condition) :: Nil
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    ReverseSubtract(source1, shifters.head, destination, condition) ::
      shifters.tail.map(value => Add(destination, value, destination, condition))
  }
}
object ReverseSubtractCarry extends DataProcessing(0x07.toByte, "rsc") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode): List[ARMOperation] = {
    if (source2 == 0)
      return ReverseSubtractCarry(source1, Shifter.RightRotateImmediate(0, 0), destination, condition) :: Nil
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    ReverseSubtractCarry(source1, shifters.head, destination, condition) ::
      shifters.tail.map(value => Add(destination, value, destination, condition))
  }
}

object SubtractCarry extends DataProcessing(0x06.toByte, "sbc") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode): List[ARMOperation] = {
    if (source2 == 0)
      return apply(source1, Shifter.RightRotateImmediate(0, 0), destination, condition) :: Nil
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    SubtractCarry(source1, shifters.head, destination, condition) ::
      shifters.tail.map(value => Subtract(destination, value, destination, condition))
  }
}
object Subtract extends DataProcessing(0x02.toByte, "sub") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode): List[ARMOperation] = {
    if (source2 == 0)
      return Nil
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    Subtract(source1, shifters.head, destination, condition) ::
      shifters.tail.map(value => Subtract(destination, value, destination, condition))
  }
}
object TestEquivalence extends DataProcessingNoDestination(0x09.toByte, "teq")
object Test extends DataProcessingNoDestination(0x08.toByte, "tst")
