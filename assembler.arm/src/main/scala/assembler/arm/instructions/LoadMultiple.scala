package assembler.arm.instructions

import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition._
import assembler.arm.operands.registers.GeneralRegister
import assembler.arm.operations.{ LoadStoreMultiple => LoadStoreMultipleOpcode }
import assembler.arm.operations.LoadStoreMultipleOperation
import assembler.arm.operations.{ ReturnFromException => ReturnFromExceptionOpcode }
import assembler.arm.operations.UpdateMode

object LoadMultiple {
  //val code: Byte = 0x08
  implicit val opcode: String = "ldm"
  private def Immed(condition: Condition, registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode, updateBase: Boolean, userModeRegisters: Boolean) =
    new LoadStoreMultipleOpcode(LoadStoreMultipleOperation.Load, condition, registers, baseRegister, addressingMode, updateBase, userModeRegisters, opcode)

  def apply(registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    Immed(condition, registers, baseRegister, addressingMode, false, false)

  def withUpdateBase(registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    Immed(condition, registers, baseRegister, addressingMode, true, false)

  def withUserModeRegisters(registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    Immed(condition, registers, baseRegister, addressingMode, false, true)

  def withUserModeRegistersAndUpdateBase(registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    Immed(condition, registers, baseRegister, addressingMode, true, true)
}

object StoreMultiple {
  implicit val opcode: String = "stm"
  private def Immed(condition: Condition, registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode, updateBase: Boolean, userModeRegisters: Boolean) =
    new LoadStoreMultipleOpcode(LoadStoreMultipleOperation.Store, condition, registers, baseRegister, addressingMode, updateBase, userModeRegisters, opcode)

  def apply(registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    Immed(condition, registers, baseRegister, addressingMode, false, false)

  def withUpdateBase(registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    Immed(condition, registers, baseRegister, addressingMode, true, false)

  def withUserModeRegisters(registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    Immed(condition, registers, baseRegister, addressingMode, false, true)
}

object ReturnFromException {
  implicit val opcode: String = "rfe"
  private def Immed(baseRegister: GeneralRegister, addressingMode: UpdateMode, updateBase: Boolean) =
    new ReturnFromExceptionOpcode(baseRegister, addressingMode, updateBase, opcode)

  def apply(baseRegister: GeneralRegister, addressingMode: UpdateMode) =
    Immed(baseRegister, addressingMode, false)

  def withUpdateBase(baseRegister: GeneralRegister, addressingMode: UpdateMode)(implicit processorMode: ProcessorMode) =
    Immed(baseRegister, addressingMode, true)
}

object Push {
  def apply(registers: List[GeneralRegister])(implicit processorMode: ProcessorMode) =
    LoadMultiple.withUpdateBase(registers, GeneralRegister.SP, UpdateMode.DecrementAfter)
}

object Pop {
  def apply(registers: List[GeneralRegister])(implicit processorMode: ProcessorMode) =
    StoreMultiple.withUpdateBase(registers, GeneralRegister.SP, UpdateMode.IncrementBefore)
}
