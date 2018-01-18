package assembler.arm.instructions

import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition.{Always, Condition}
import assembler.arm.operands.registers.GeneralRegister
import assembler.arm.operations.{MultiplyOperation, MultiplyWithRegisterOperation, SetFlags}

class MultiplyWithRegister(val code: Byte, val opcode: String) {

  def apply(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, addValue: GeneralRegister,
            condition: Condition = Always)(implicit processorMode: ProcessorMode): MultiplyWithRegisterOperation =
    RegRegAndRegToReg(destination, source, multiplyValue, addValue, condition)

  private def RegRegAndRegToReg(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister,
                                addValue: GeneralRegister, condition: Condition) =
    new MultiplyWithRegisterOperation(code, opcode, destination, source, multiplyValue, addValue, condition)

  def setFlags(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, addValue: GeneralRegister,
               condition: Condition = Always)(implicit processorMode: ProcessorMode): MultiplyWithRegisterOperation with SetFlags =
    RegRegAndRegToRegFlags(destination, source, multiplyValue, addValue, condition)

  private def RegRegAndRegToRegFlags(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister,
                                     addValue: GeneralRegister, condition: Condition) =
    new MultiplyWithRegisterOperation(code, opcode, destination, source, multiplyValue, addValue, condition) with SetFlags
}

class Multiply(val code: Byte, val opcode: String) {
  def apply(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, condition: Condition = Always)
           (implicit processorMode: ProcessorMode): MultiplyOperation =
    RegAndRegToReg(destination, source, multiplyValue, condition)

  private def RegAndRegToReg(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, condition: Condition) =
    new MultiplyOperation(code, opcode, destination, source, multiplyValue, condition)

  def setFlags(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister, condition: Condition = Always)
              (implicit processorMode: ProcessorMode): MultiplyOperation with SetFlags =
    RegAndRegToRegFlags(destination, source, multiplyValue, condition)

  private def RegAndRegToRegFlags(destination: GeneralRegister, source: GeneralRegister, multiplyValue: GeneralRegister,
                                  condition: Condition) =
    new MultiplyOperation(code, opcode, destination, source, multiplyValue, condition) with SetFlags
}

object MultiplyAccumulate extends MultiplyWithRegister(0x01.toByte, "mla")

object Multiply extends Multiply(0x00.toByte, "mul")