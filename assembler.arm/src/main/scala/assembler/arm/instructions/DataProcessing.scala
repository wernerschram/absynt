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

object AddCarry extends DataProcessing(0x05.toByte, "adc")
object Add extends DataProcessing(0x04.toByte, "add")
object And extends DataProcessing(0x00.toByte, "and")
object BitClear extends DataProcessing(0x0E.toByte, "bic")
object CompareNegative extends DataProcessingNoDestination(0x0B.toByte, "cmn")
object Compare extends DataProcessingNoDestination(0x0A.toByte, "cmp")
object ExclusiveOr extends DataProcessing(0x01.toByte, "eor")
object Move extends DataProcessingNoRegister(0x0D.toByte, "mov") {
  def forShifters(source2: List[RightRotateImmediate], destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    apply(source2.head, destination, condition) :: source2.tail.map(value => Or(destination, value, destination, condition))
}
object MoveNot extends DataProcessingNoRegister(0x0F.toByte, "mvn")
object Or extends DataProcessing(0x0C.toByte, "orr") {
  def forShifters(source1: GeneralRegister, source2: List[RightRotateImmediate], destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    apply(source1, source2.head, destination, condition) :: source2.tail.map(value => Or(destination, value, destination, condition))
}
object ReverseSubtract extends DataProcessing(0x03.toByte, "rsb")
object ReverseSubtractCarry extends DataProcessing(0x07.toByte, "rsc")
object SubtractCarry extends DataProcessing(0x06.toByte, "sbc")
object Subtract extends DataProcessing(0x02.toByte, "sub")
object TestEquivalence extends DataProcessingNoDestination(0x09.toByte, "teq")
object Test extends DataProcessingNoDestination(0x08.toByte, "tst")
