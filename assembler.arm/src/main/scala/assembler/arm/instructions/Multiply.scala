package assembler.arm.instructions

import assembler.arm.ProcessorMode
import assembler.arm.opcodes.{ Multiply => MultiplyOpcode }
import assembler.arm.operands.Condition._
import assembler.arm.operands.registers.GeneralRegister

class MultiplyWithRegister(val code: Byte, val opcode: String) {
  private val RegRegAndRegToReg = new MultiplyOpcode(code)(opcode)

  def apply(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, addValue: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    RegRegAndRegToReg(destination, source, multiplyValue, addValue, condition)

  def setFlags(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, addValue: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    RegRegAndRegToReg.setFlags(destination, source, multiplyValue, addValue, condition)
}

class Multiply(val code: Byte, val opcode: String) {
  private val RegAndRegToReg = new MultiplyOpcode(code)(opcode)

  def apply(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    RegAndRegToReg(destination, source, multiplyValue, condition)

  def setFlags(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    RegAndRegToReg.setFlags(destination, source, multiplyValue, condition)
}

object MultiplyAccumulate extends MultiplyWithRegister(0x01.toByte, "mla")
object Multiply extends Multiply(0x00.toByte, "mul")