package assembler.arm.instructions

import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition.{Always, Condition}
import assembler.arm.operands.registers.GeneralRegister
import assembler.arm.operations.{MultiplyOperation, MultiplyWithRegisterOperation, SetFlags}

class MultiplyWithRegister(val code: Byte, val opcode: String) {

  private def RegRegAndRegToReg(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, addValue: GeneralRegister, condition: Condition) =
    new MultiplyWithRegisterOperation(code, opcode, destination, source, multiplyValue, addValue, condition)

  private def RegRegAndRegToRegFlags(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, addValue: GeneralRegister, condition: Condition) =
    new MultiplyWithRegisterOperation(code, opcode, destination, source, multiplyValue, addValue, condition) with SetFlags

  def apply(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, addValue: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    RegRegAndRegToReg(destination, source, multiplyValue, addValue, condition)

  def setFlags(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, addValue: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    RegRegAndRegToRegFlags(destination, source, multiplyValue, addValue, condition)
}

class Multiply(val code: Byte, val opcode: String) {
  private def RegAndRegToReg(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, condition: Condition) =
    new MultiplyOperation(code, opcode, destination, source, multiplyValue, condition)

  private def RegAndRegToRegFlags(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, condition: Condition) =
    new MultiplyOperation(code, opcode, destination, source, multiplyValue, condition) with SetFlags

  def apply(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    RegAndRegToReg(destination, source, multiplyValue, condition)

  def setFlags(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    RegAndRegToRegFlags(destination, source, multiplyValue, condition)
}

object MultiplyAccumulate extends MultiplyWithRegister(0x01.toByte, "mla")
object Multiply extends Multiply(0x00.toByte, "mul")