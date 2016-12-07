package assembler.arm.instructions

import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition._
import assembler.arm.operands.registers._
import assembler.arm.operands.RightRotateImmediate
import assembler.arm.operands.Shifter
import assembler.arm.operations.Fields
import assembler.arm.operations.{ MoveFromStatusRegister => MoveFromStatusRegisterOpcode }
import assembler.arm.operations.{ MoveToStatusRegister => MoveToStatusRegisterOpcode }

object MoveFromStatusRegister {
  implicit val opcode = "mrs"

  private def RegToStatus(source: StatusRegister, destination: GeneralRegister, condition: Condition) =
    new MoveFromStatusRegisterOpcode(opcode, source, destination, condition)

  def apply(source: StatusRegister, destination: GeneralRegister, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    RegToStatus(source, destination, condition)
}

object MoveToStatusRegister {
  implicit val opcode = "msr"

  private def RegToReg(source: GeneralRegister, destination: StatusRegister, fields: Fields.ValueSet, condition: Condition) =
    new MoveToStatusRegisterOpcode(opcode, source, destination, fields, condition)

  private def ImmediateToReg(source: RightRotateImmediate, destination: StatusRegister, fields: Fields.ValueSet, condition: Condition) =
    new MoveToStatusRegisterOpcode(opcode, source, destination, fields, condition)

  def apply(source: GeneralRegister, destination: StatusRegister, fields: Fields.ValueSet)(implicit processorMode: ProcessorMode) =
    RegToReg(source, destination, fields, Always)

  def apply(source: GeneralRegister, destination: StatusRegister, fields: Fields.ValueSet, condition: Condition)(implicit processorMode: ProcessorMode) =
    RegToReg(source, destination, fields, condition)

  def apply(source: RightRotateImmediate, destination: StatusRegister, fields: Fields.ValueSet)(implicit processorMode: ProcessorMode) =
    ImmediateToReg(source, destination, fields, Always)

  def apply(source: RightRotateImmediate, destination: StatusRegister, fields: Fields.ValueSet, condition: Condition)(implicit processorMode: ProcessorMode) =
    ImmediateToReg(source, destination, fields, condition)
}