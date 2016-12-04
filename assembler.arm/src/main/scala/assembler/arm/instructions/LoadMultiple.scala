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
  private val Immed = new LoadStoreMultipleOpcode(LoadStoreMultipleOperation.Load)

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
  private val Immed = new LoadStoreMultipleOpcode(LoadStoreMultipleOperation.Store)

  def apply(registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    Immed(condition, registers, baseRegister, addressingMode, false, false)

  def withUpdateBase(registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    Immed(condition, registers, baseRegister, addressingMode, true, false)

  def withUserModeRegisters(registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    Immed(condition, registers, baseRegister, addressingMode, false, true)
}

object ReturnFromException {
  implicit val opcode: String = "rfe"
  private val Immed = new ReturnFromExceptionOpcode()

  def apply(baseRegister: GeneralRegister, addressingMode: UpdateMode)(implicit processorMode: ProcessorMode) =
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
