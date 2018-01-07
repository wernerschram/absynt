package assembler.arm.instructions

import assembler.Label
import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition._
import assembler.arm.operands.registers.GeneralRegister
import assembler.arm.operations.{LoadStoreMultiple, LoadStoreMultipleDirection, UpdateBase, UpdateMode, UserModeRegisters, ReturnFromException => ReturnFromExceptionOperation}

object LoadMultiple {
  //val code: Byte = 0x08
  implicit val opcode: String = "ldm"

  def apply(registers: Seq[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode, condition: Condition = Always)
           (implicit processorMode: ProcessorMode, label: Label): LoadStoreMultiple =
    Immed(label, condition, registers, baseRegister, addressingMode)

  private def Immed(label: Label, condition: Condition, registers: Seq[GeneralRegister], baseRegister: GeneralRegister,
                    addressingMode: UpdateMode) =
    new LoadStoreMultiple(label, LoadStoreMultipleDirection.Load, condition, registers, baseRegister, addressingMode, opcode)

  def withUpdateBase(registers: Seq[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode,
                     condition: Condition = Always)(implicit processorMode: ProcessorMode, label: Label): LoadStoreMultiple with UpdateBase =
    ImmedUpdateBase(label, condition, registers, baseRegister, addressingMode)

  private def ImmedUpdateBase(label: Label, condition: Condition, registers: Seq[GeneralRegister], baseRegister: GeneralRegister,
                              addressingMode: UpdateMode) =
    new LoadStoreMultiple(label, LoadStoreMultipleDirection.Load, condition, registers, baseRegister, addressingMode, opcode) with UpdateBase

  def withUserModeRegisters(registers: Seq[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode,
                            condition: Condition = Always)(implicit processorMode: ProcessorMode, label: Label): LoadStoreMultiple with UserModeRegisters =
    ImmedUserModeRegisters(label, condition, registers, baseRegister, addressingMode)

  private def ImmedUserModeRegisters(label: Label, condition: Condition, registers: Seq[GeneralRegister], baseRegister: GeneralRegister,
                                     addressingMode: UpdateMode) =
    new LoadStoreMultiple(label, LoadStoreMultipleDirection.Load, condition, registers, baseRegister, addressingMode, opcode)
      with UserModeRegisters

  def withUserModeRegistersAndUpdateBase(registers: Seq[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode,
                                         condition: Condition = Always)(implicit processorMode: ProcessorMode, label: Label): LoadStoreMultiple with UpdateBase with UserModeRegisters =
    ImmedUserModeRegistersAndUpdateBase(label, condition, registers, baseRegister, addressingMode)

  private def ImmedUserModeRegistersAndUpdateBase(label: Label, condition: Condition, registers: Seq[GeneralRegister],
                                                  baseRegister: GeneralRegister, addressingMode: UpdateMode) =
    new LoadStoreMultiple(label, LoadStoreMultipleDirection.Load, condition, registers, baseRegister, addressingMode, opcode)
      with UpdateBase with UserModeRegisters
}

object StoreMultiple {
  implicit val opcode: String = "stm"

  def apply(registers: Seq[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode, condition: Condition = Always)
           (implicit processorMode: ProcessorMode, label: Label): LoadStoreMultiple =
    Immed(label, condition, registers, baseRegister, addressingMode)

  private def Immed(label: Label, condition: Condition, registers: Seq[GeneralRegister], baseRegister: GeneralRegister,
                    addressingMode: UpdateMode) =
    new LoadStoreMultiple(label, LoadStoreMultipleDirection.Store, condition, registers, baseRegister, addressingMode, opcode)

  def withUpdateBase(registers: Seq[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode,
                     condition: Condition = Always)(implicit processorMode: ProcessorMode, label: Label): LoadStoreMultiple with UpdateBase =
    ImmedUpdateBase(label, condition, registers, baseRegister, addressingMode)

  private def ImmedUpdateBase(label: Label, condition: Condition, registers: Seq[GeneralRegister], baseRegister: GeneralRegister,
                              addressingMode: UpdateMode) =
    new LoadStoreMultiple(label, LoadStoreMultipleDirection.Store, condition, registers, baseRegister, addressingMode, opcode)
      with UpdateBase

  def withUserModeRegisters(registers: Seq[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode,
                            condition: Condition = Always)(implicit processorMode: ProcessorMode, label: Label): LoadStoreMultiple with UserModeRegisters =
    ImmedUserModeRegisters(label, condition, registers, baseRegister, addressingMode)

  private def ImmedUserModeRegisters(label: Label, condition: Condition, registers: Seq[GeneralRegister], baseRegister: GeneralRegister,
                                     addressingMode: UpdateMode) =
    new LoadStoreMultiple(label, LoadStoreMultipleDirection.Store, condition, registers, baseRegister, addressingMode, opcode)
      with UserModeRegisters
}

object ReturnFromException {
  implicit val opcode: String = "rfe"

  def apply(baseRegister: GeneralRegister, addressingMode: UpdateMode)(implicit processorMode: ProcessorMode, label: Label): ReturnFromExceptionOperation =
    Immed(label, baseRegister, addressingMode, updateBase = false)

  private def Immed(label: Label, baseRegister: GeneralRegister, addressingMode: UpdateMode, updateBase: Boolean) =
    new ReturnFromExceptionOperation(label, baseRegister, addressingMode, updateBase, opcode)

  def withUpdateBase(baseRegister: GeneralRegister, addressingMode: UpdateMode)(implicit processorMode: ProcessorMode, label: Label): ReturnFromExceptionOperation =
    Immed(label, baseRegister, addressingMode, updateBase = true)
}

object Push {
  def apply(registers: Seq[GeneralRegister])(implicit processorMode: ProcessorMode, label: Label): LoadStoreMultiple with UpdateBase =
    LoadMultiple.withUpdateBase(registers, GeneralRegister.SP, UpdateMode.DecrementAfter)
}

object Pop {
  def apply(registers: Seq[GeneralRegister])(implicit processorMode: ProcessorMode, label: Label): LoadStoreMultiple with UpdateBase =
    StoreMultiple.withUpdateBase(registers, GeneralRegister.SP, UpdateMode.IncrementBefore)
}
