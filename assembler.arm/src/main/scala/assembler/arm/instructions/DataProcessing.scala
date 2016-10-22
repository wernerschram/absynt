package assembler.arm.instructions

import assembler.arm.ProcessorMode
import assembler.arm.opcodes.{ DataProcessing => DataProcessingOpcode }
import assembler.arm.operands.Condition._
import assembler.arm.operands.RightRotateImmediate
import assembler.arm.operands.Shifter
import assembler.arm.operands.registers.GeneralRegister

class DataProcessing(val code: Byte, val opcode: String) {
  private val RegAndShifterToReg = new DataProcessingOpcode(code)(opcode)

  def apply(source1: GeneralRegister, source2: Shifter, destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    RegAndShifterToReg(source1, source2, destination, condition)

  def setFlags(source1: GeneralRegister, source2: Shifter, destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    RegAndShifterToReg.setFlags(source1, source2, destination, condition)
}

class DataProcessingNoDestination(val code: Byte, val opcode: String) {
  private val RegAndShifter = new DataProcessingOpcode(code)(opcode)

  def apply(register1: GeneralRegister, source2: Shifter, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    RegAndShifter(register1, source2, condition)

}

class DataProcessingNoRegister(val code: Byte, val opcode: String) {
  private val ShifterToReg = new DataProcessingOpcode(code)(opcode)

  def apply(source2: Shifter, destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    ShifterToReg(source2, destination, condition)

  def setFlags(source2: Shifter, destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    ShifterToReg.setFlags(source2, destination, condition)
}

object AddCarry extends DataProcessing(0x05.toByte, "adc") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode): List[ARMInstruction] = {
    if (source2 == 0)
      return apply(source1, Shifter.RightRotateImmediate(0, 0), destination, condition) :: Nil
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    apply(source1, shifters.head, destination, condition) ::
      shifters.tail.map(value => Add(destination, value, destination, condition))
  }
}
object Add extends DataProcessing(0x04.toByte, "add") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode): List[ARMInstruction] = {
    if (source2 == 0)
      return apply(source1, Shifter.RightRotateImmediate(0, 0), destination, condition) :: Nil
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    apply(source1, shifters.head, destination, condition) ::
      shifters.tail.map(value => Add(destination, value, destination, condition))
  }
}
object And extends DataProcessing(0x00.toByte, "and") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode): List[ARMInstruction] = {
    if (source2 == 0)
      return apply(source1, Shifter.RightRotateImmediate(0, 0), destination, condition) :: Nil
    val shifters: List[RightRotateImmediate] = Shifter.apply(~source2)
    BitClear(source1, shifters.head, destination, condition) ::
      shifters.tail.map(value => BitClear(destination, value, destination, condition))
  }
}
object BitClear extends DataProcessing(0x0E.toByte, "bic") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode): List[ARMInstruction] = {
    if (source2 == 0)
      return apply(source1, Shifter.RightRotateImmediate(0, 0), destination, condition) :: Nil
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    BitClear(source1, shifters.head, destination, condition) ::
      shifters.tail.map(value => BitClear(destination, value, destination, condition))
  }
}
object CompareNegative extends DataProcessingNoDestination(0x0B.toByte, "cmn")
object Compare extends DataProcessingNoDestination(0x0A.toByte, "cmp")
object ExclusiveOr extends DataProcessing(0x01.toByte, "eor") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode): List[ARMInstruction] = {
    if (source2 == 0)
      return apply(source1, Shifter.RightRotateImmediate(0, 0), destination, condition) :: Nil
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    ExclusiveOr(source1, shifters.head, destination, condition) ::
      shifters.tail.map(value => ExclusiveOr(destination, value, destination, condition))
  }
}
object Move extends DataProcessingNoRegister(0x0D.toByte, "mov") {
  def forConstant(source2: Int, destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode): List[ARMInstruction] = {
    if (source2 == 0)
      return apply(Shifter.RightRotateImmediate(0, 0), destination, condition) :: Nil
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
      apply(shifters.head, destination, condition) :: shifters.tail.map(value => Or(destination, value, destination, condition))
  }
}
object MoveNot extends DataProcessingNoRegister(0x0F.toByte, "mvn")
object Or extends DataProcessing(0x0C.toByte, "orr") {
  def forConstant(source1: GeneralRegister, source2: Int, destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode): List[ARMInstruction] = {
    if (source2 == 0)
      return apply(source1, Shifter.RightRotateImmediate(0, 0), destination, condition) :: Nil
    val shifters: List[RightRotateImmediate] = Shifter.apply(source2)
    apply(source1, shifters.head, destination, condition) ::
      shifters.tail.map(value => Or(destination, value, destination, condition))
  }
}
object ReverseSubtract extends DataProcessing(0x03.toByte, "rsb")
object ReverseSubtractCarry extends DataProcessing(0x07.toByte, "rsc")
object SubtractCarry extends DataProcessing(0x06.toByte, "sbc")
object Subtract extends DataProcessing(0x02.toByte, "sub")
object TestEquivalence extends DataProcessingNoDestination(0x09.toByte, "teq")
object Test extends DataProcessingNoDestination(0x08.toByte, "tst")
