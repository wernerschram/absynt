package assembler.arm.instructions

import assembler.Label
import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition._
import assembler.arm.operands.registers.GeneralRegister
import assembler.arm.operations.{LoadStoreMultipleDirection, UpdateBase, UpdateMode, UserModeRegisters, LoadStoreMultiple => LoadStoreMultipleOpcode, ReturnFromException => ReturnFromExceptionOpcode}

object LoadMultiple {
  //val code: Byte = 0x08
  implicit val opcode: String = "ldm"

  def apply(registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode, condition: Condition = Always)
           (implicit processorMode: ProcessorMode, label: Label) =
    Immed(label, condition, registers, baseRegister, addressingMode)

  private def Immed(label: Label, condition: Condition, registers: List[GeneralRegister], baseRegister: GeneralRegister,
                    addressingMode: UpdateMode) =
    new LoadStoreMultipleOpcode(label, LoadStoreMultipleDirection.Load, condition, registers, baseRegister, addressingMode, opcode)

  def withUpdateBase(registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode,
                     condition: Condition = Always)(implicit processorMode: ProcessorMode, label: Label) =
    ImmedUpdateBase(label, condition, registers, baseRegister, addressingMode)

  private def ImmedUpdateBase(label: Label, condition: Condition, registers: List[GeneralRegister], baseRegister: GeneralRegister,
                              addressingMode: UpdateMode) =
    new LoadStoreMultipleOpcode(label, LoadStoreMultipleDirection.Load, condition, registers, baseRegister, addressingMode, opcode) with UpdateBase

  def withUserModeRegisters(registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode,
                            condition: Condition = Always)(implicit processorMode: ProcessorMode, label: Label) =
    ImmedUserModeRegisters(label, condition, registers, baseRegister, addressingMode)

  private def ImmedUserModeRegisters(label: Label, condition: Condition, registers: List[GeneralRegister], baseRegister: GeneralRegister,
                                     addressingMode: UpdateMode) =
    new LoadStoreMultipleOpcode(label, LoadStoreMultipleDirection.Load, condition, registers, baseRegister, addressingMode, opcode)
      with UserModeRegisters

  def withUserModeRegistersAndUpdateBase(registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode,
                                         condition: Condition = Always)(implicit processorMode: ProcessorMode, label: Label) =
    ImmedUserModeRegistersAndUpdateBase(label, condition, registers, baseRegister, addressingMode)

  private def ImmedUserModeRegistersAndUpdateBase(label: Label, condition: Condition, registers: List[GeneralRegister],
                                                  baseRegister: GeneralRegister, addressingMode: UpdateMode) =
    new LoadStoreMultipleOpcode(label, LoadStoreMultipleDirection.Load, condition, registers, baseRegister, addressingMode, opcode)
      with UpdateBase with UserModeRegisters
}

object StoreMultiple {
  implicit val opcode: String = "stm"

  def apply(registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode, condition: Condition = Always)
           (implicit processorMode: ProcessorMode, label: Label) =
    Immed(label, condition, registers, baseRegister, addressingMode)

  private def Immed(label: Label, condition: Condition, registers: List[GeneralRegister], baseRegister: GeneralRegister,
                    addressingMode: UpdateMode) =
    new LoadStoreMultipleOpcode(label, LoadStoreMultipleDirection.Store, condition, registers, baseRegister, addressingMode, opcode)

  def withUpdateBase(registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode,
                     condition: Condition = Always)(implicit processorMode: ProcessorMode, label: Label) =
    ImmedUpdateBase(label, condition, registers, baseRegister, addressingMode)

  private def ImmedUpdateBase(label: Label, condition: Condition, registers: List[GeneralRegister], baseRegister: GeneralRegister,
                              addressingMode: UpdateMode) =
    new LoadStoreMultipleOpcode(label, LoadStoreMultipleDirection.Store, condition, registers, baseRegister, addressingMode, opcode)
      with UpdateBase

  def withUserModeRegisters(registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode,
                            condition: Condition = Always)(implicit processorMode: ProcessorMode, label: Label) =
    ImmedUserModeRegisters(label, condition, registers, baseRegister, addressingMode)

  private def ImmedUserModeRegisters(label: Label, condition: Condition, registers: List[GeneralRegister], baseRegister: GeneralRegister,
                                     addressingMode: UpdateMode) =
    new LoadStoreMultipleOpcode(label, LoadStoreMultipleDirection.Store, condition, registers, baseRegister, addressingMode, opcode)
      with UserModeRegisters
}

object ReturnFromException {
  implicit val opcode: String = "rfe"

  def apply(baseRegister: GeneralRegister, addressingMode: UpdateMode)(implicit processorMode: ProcessorMode, label: Label) =
    Immed(label, baseRegister, addressingMode, updateBase = false)

  private def Immed(label: Label, baseRegister: GeneralRegister, addressingMode: UpdateMode, updateBase: Boolean) =
    new ReturnFromExceptionOpcode(label, baseRegister, addressingMode, updateBase, opcode)

  def withUpdateBase(baseRegister: GeneralRegister, addressingMode: UpdateMode)(implicit processorMode: ProcessorMode, label: Label) =
    Immed(label, baseRegister, addressingMode, updateBase = true)
}

object Push {
  def apply(registers: List[GeneralRegister])(implicit processorMode: ProcessorMode, label: Label): LoadStoreMultipleOpcode with UpdateBase =
    LoadMultiple.withUpdateBase(registers, GeneralRegister.SP, UpdateMode.DecrementAfter)
}

object Pop {
  def apply(registers: List[GeneralRegister])(implicit processorMode: ProcessorMode, label: Label): LoadStoreMultipleOpcode with UpdateBase =
    StoreMultiple.withUpdateBase(registers, GeneralRegister.SP, UpdateMode.IncrementBefore)
}
