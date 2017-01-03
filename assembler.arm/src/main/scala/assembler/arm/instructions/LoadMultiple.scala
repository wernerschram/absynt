package assembler.arm.instructions

import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition._
import assembler.arm.operands.registers.GeneralRegister
import assembler.arm.operations.{LoadStoreMultipleDirection, UpdateBase, UpdateMode, UserModeRegisters,
                                 LoadStoreMultiple => LoadStoreMultipleOpcode, ReturnFromException => ReturnFromExceptionOpcode}

object LoadMultiple {
  //val code: Byte = 0x08
  implicit val opcode: String = "ldm"

  def apply(registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode, condition: Condition = Always)
           (implicit processorMode: ProcessorMode) =
    Immed(condition, registers, baseRegister, addressingMode)

  private def Immed(condition: Condition, registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode) =
    new LoadStoreMultipleOpcode(LoadStoreMultipleDirection.Load, condition, registers, baseRegister, addressingMode, opcode)

  def withUpdateBase(registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode,
                     condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    ImmedUpdateBase(condition, registers, baseRegister, addressingMode)

  private def ImmedUpdateBase(condition: Condition, registers: List[GeneralRegister], baseRegister: GeneralRegister,
                              addressingMode: UpdateMode) =
    new LoadStoreMultipleOpcode(LoadStoreMultipleDirection.Load, condition, registers, baseRegister, addressingMode, opcode) with UpdateBase

  def withUserModeRegisters(registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode,
                            condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    ImmedUserModeRegisters(condition, registers, baseRegister, addressingMode)

  private def ImmedUserModeRegisters(condition: Condition, registers: List[GeneralRegister], baseRegister: GeneralRegister,
                                     addressingMode: UpdateMode) =
    new LoadStoreMultipleOpcode(LoadStoreMultipleDirection.Load, condition, registers, baseRegister, addressingMode, opcode)
      with UserModeRegisters

  def withUserModeRegistersAndUpdateBase(registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode,
                                         condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    ImmedUserModeRegistersAndUpdateBase(condition, registers, baseRegister, addressingMode)

  private def ImmedUserModeRegistersAndUpdateBase(condition: Condition, registers: List[GeneralRegister], baseRegister: GeneralRegister,
                                                  addressingMode: UpdateMode) =
    new LoadStoreMultipleOpcode(LoadStoreMultipleDirection.Load, condition, registers, baseRegister, addressingMode, opcode)
      with UpdateBase with UserModeRegisters
}

object StoreMultiple {
  implicit val opcode: String = "stm"

  def apply(registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode, condition: Condition = Always)
           (implicit processorMode: ProcessorMode) =
    Immed(condition, registers, baseRegister, addressingMode)

  private def Immed(condition: Condition, registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode) =
    new LoadStoreMultipleOpcode(LoadStoreMultipleDirection.Store, condition, registers, baseRegister, addressingMode, opcode)

  def withUpdateBase(registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode, condition:
  Condition = Always)(implicit processorMode: ProcessorMode) =
    ImmedUpdateBase(condition, registers, baseRegister, addressingMode)

  private def ImmedUpdateBase(condition: Condition, registers: List[GeneralRegister], baseRegister: GeneralRegister,
                              addressingMode: UpdateMode) =
    new LoadStoreMultipleOpcode(LoadStoreMultipleDirection.Store, condition, registers, baseRegister, addressingMode, opcode)
      with UpdateBase

  def withUserModeRegisters(registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode,
                            condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    ImmedUserModeRegisters(condition, registers, baseRegister, addressingMode)

  private def ImmedUserModeRegisters(condition: Condition, registers: List[GeneralRegister], baseRegister: GeneralRegister,
                                     addressingMode: UpdateMode) =
    new LoadStoreMultipleOpcode(LoadStoreMultipleDirection.Store, condition, registers, baseRegister, addressingMode, opcode)
      with UserModeRegisters
}

object ReturnFromException {
  implicit val opcode: String = "rfe"

  def apply(baseRegister: GeneralRegister, addressingMode: UpdateMode) =
    Immed(baseRegister, addressingMode, updateBase = false)

  private def Immed(baseRegister: GeneralRegister, addressingMode: UpdateMode, updateBase: Boolean) =
    new ReturnFromExceptionOpcode(baseRegister, addressingMode, updateBase, opcode)

  def withUpdateBase(baseRegister: GeneralRegister, addressingMode: UpdateMode)(implicit processorMode: ProcessorMode) =
    Immed(baseRegister, addressingMode, updateBase = true)
}

object Push {
  def apply(registers: List[GeneralRegister])(implicit processorMode: ProcessorMode): LoadStoreMultipleOpcode with UpdateBase =
    LoadMultiple.withUpdateBase(registers, GeneralRegister.SP, UpdateMode.DecrementAfter)
}

object Pop {
  def apply(registers: List[GeneralRegister])(implicit processorMode: ProcessorMode): LoadStoreMultipleOpcode with UpdateBase =
    StoreMultiple.withUpdateBase(registers, GeneralRegister.SP, UpdateMode.IncrementBefore)
}
