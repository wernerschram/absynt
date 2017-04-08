package assembler.arm.instructions

import assembler.Label
import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition.{Always, Condition}
import assembler.arm.operands.registers.GeneralRegister
import assembler.arm.operations.{MultiplyOperation, MultiplyWithRegisterOperation, SetFlags}

class MultiplyWithRegister(val code: Byte, val opcode: String) {

  def apply(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, addValue: GeneralRegister,
            condition: Condition = Always)(implicit processorMode: ProcessorMode, label: Label) =
    RegRegAndRegToReg(label, destination, source, multiplyValue, addValue, condition)

  private def RegRegAndRegToReg(label: Label, destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister,
                                addValue: GeneralRegister, condition: Condition) =
    new MultiplyWithRegisterOperation(label, code, opcode, destination, source, multiplyValue, addValue, condition)

  def setFlags(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, addValue: GeneralRegister,
               condition: Condition = Always)(implicit processorMode: ProcessorMode, label: Label) =
    RegRegAndRegToRegFlags(label, destination, source, multiplyValue, addValue, condition)

  private def RegRegAndRegToRegFlags(label: Label, destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister,
                                     addValue: GeneralRegister, condition: Condition) =
    new MultiplyWithRegisterOperation(label, code, opcode, destination, source, multiplyValue, addValue, condition) with SetFlags
}

class Multiply(val code: Byte, val opcode: String) {
  def apply(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, condition: Condition = Always)
           (implicit processorMode: ProcessorMode, label: Label) =
    RegAndRegToReg(label, destination, source, multiplyValue, condition)

  private def RegAndRegToReg(label: Label, destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, condition: Condition) =
    new MultiplyOperation(label, code, opcode, destination, source, multiplyValue, condition)

  def setFlags(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, condition: Condition = Always)
              (implicit processorMode: ProcessorMode, label: Label) =
    RegAndRegToRegFlags(label, destination, source, multiplyValue, condition)

  private def RegAndRegToRegFlags(label: Label, destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister,
                                  condition: Condition) =
    new MultiplyOperation(label, code, opcode, destination, source, multiplyValue, condition) with SetFlags
}

object MultiplyAccumulate extends MultiplyWithRegister(0x01.toByte, "mla")

object Multiply extends Multiply(0x00.toByte, "mul")