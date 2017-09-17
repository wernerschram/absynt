package assembler.arm.instructions

import assembler.Label
import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition._
import assembler.arm.operands.RightRotateImmediate
import assembler.arm.operands.registers._
import assembler.arm.operations.{Fields, MoveFromStatusRegister => MoveFromStatusRegisterOperation, MoveToStatusRegister => MoveToStatusRegisterOpcode}

object MoveFromStatusRegister {
  implicit val opcode: String = "mrs"

  def apply(source: StatusRegister, destination: GeneralRegister, condition: Condition = Always)(implicit label: Label, processorMode: ProcessorMode): MoveFromStatusRegisterOperation =
    RegToStatus(label, source, destination, condition)

  private def RegToStatus(label: Label, source: StatusRegister, destination: GeneralRegister, condition: Condition) =
    new MoveFromStatusRegisterOperation(label, opcode, source, destination, condition)
}

object MoveToStatusRegister {
  implicit val opcode: String = "msr"

  def apply(source: GeneralRegister, destination: StatusRegister, fields: Fields.ValueSet)(implicit label: Label, processorMode: ProcessorMode): MoveToStatusRegisterOpcode =
    RegToReg(label, source, destination, fields, Always)

  def apply(source: GeneralRegister, destination: StatusRegister, fields: Fields.ValueSet, condition: Condition)
           (implicit label: Label, processorMode: ProcessorMode): MoveToStatusRegisterOpcode =
    RegToReg(label, source, destination, fields, condition)

  private def RegToReg(label: Label, source: GeneralRegister, destination: StatusRegister, fields: Fields.ValueSet, condition: Condition) =
    new MoveToStatusRegisterOpcode(label, opcode, source, destination, fields, condition)

  def apply(source: RightRotateImmediate, destination: StatusRegister, fields: Fields.ValueSet)(implicit label: Label, processorMode: ProcessorMode): MoveToStatusRegisterOpcode =
    ImmediateToReg(label, source, destination, fields, Always)

  def apply(source: RightRotateImmediate, destination: StatusRegister, fields: Fields.ValueSet, condition: Condition)
           (implicit label: Label, processorMode: ProcessorMode): MoveToStatusRegisterOpcode =
    ImmediateToReg(label, source, destination, fields, condition)

  private def ImmediateToReg(label: Label, source: RightRotateImmediate, destination: StatusRegister, fields: Fields.ValueSet, condition: Condition) =
    new MoveToStatusRegisterOpcode(label, opcode, source, destination, fields, condition)
}