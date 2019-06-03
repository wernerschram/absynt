package org.werner.absynt.arm.instructions

import org.werner.absynt.arm.ProcessorMode
import org.werner.absynt.arm.operands.Condition._
import org.werner.absynt.arm.operands.registers.GeneralRegister
import org.werner.absynt.arm.operations.{LoadStoreMultiple, LoadStoreMultipleDirection, UpdateBase, UpdateMode, UserModeRegisters, ReturnFromException => ReturnFromExceptionOperation}

object LoadMultiple {
  implicit val opcode: String = "ldm"

  def apply(registers: Seq[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode, condition: Condition = Always)
           (implicit processorMode: ProcessorMode): LoadStoreMultiple =
    Immed(condition, registers, baseRegister, addressingMode)

  private def Immed(condition: Condition, registers: Seq[GeneralRegister], baseRegister: GeneralRegister,
                    addressingMode: UpdateMode) =
    new LoadStoreMultiple(LoadStoreMultipleDirection.Load, condition, registers, baseRegister, addressingMode, opcode)

  def withUpdateBase(registers: Seq[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode,
                     condition: Condition = Always)(implicit processorMode: ProcessorMode): LoadStoreMultiple with UpdateBase =
    ImmedUpdateBase(condition, registers, baseRegister, addressingMode)

  private def ImmedUpdateBase(condition: Condition, registers: Seq[GeneralRegister], baseRegister: GeneralRegister,
                              addressingMode: UpdateMode) =
    new LoadStoreMultiple(LoadStoreMultipleDirection.Load, condition, registers, baseRegister, addressingMode, opcode) with UpdateBase

  def withUserModeRegisters(registers: Seq[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode,
                            condition: Condition = Always)(implicit processorMode: ProcessorMode): LoadStoreMultiple with UserModeRegisters =
    ImmedUserModeRegisters(condition, registers, baseRegister, addressingMode)

  private def ImmedUserModeRegisters(condition: Condition, registers: Seq[GeneralRegister], baseRegister: GeneralRegister,
                                     addressingMode: UpdateMode) =
    new LoadStoreMultiple(LoadStoreMultipleDirection.Load, condition, registers, baseRegister, addressingMode, opcode)
      with UserModeRegisters

  def withUserModeRegistersAndUpdateBase(registers: Seq[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode,
                                         condition: Condition = Always)(implicit processorMode: ProcessorMode): LoadStoreMultiple with UpdateBase with UserModeRegisters =
    ImmedUserModeRegistersAndUpdateBase(condition, registers, baseRegister, addressingMode)

  private def ImmedUserModeRegistersAndUpdateBase(condition: Condition, registers: Seq[GeneralRegister],
                                                  baseRegister: GeneralRegister, addressingMode: UpdateMode) =
    new LoadStoreMultiple(LoadStoreMultipleDirection.Load, condition, registers, baseRegister, addressingMode, opcode)
      with UpdateBase with UserModeRegisters
}

object StoreMultiple {
  implicit val opcode: String = "stm"

  def apply(registers: Seq[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode, condition: Condition = Always)
           (implicit processorMode: ProcessorMode): LoadStoreMultiple =
    Immed(condition, registers, baseRegister, addressingMode)

  private def Immed(condition: Condition, registers: Seq[GeneralRegister], baseRegister: GeneralRegister,
                    addressingMode: UpdateMode) =
    new LoadStoreMultiple(LoadStoreMultipleDirection.Store, condition, registers, baseRegister, addressingMode, opcode)

  def withUpdateBase(registers: Seq[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode,
                     condition: Condition = Always)(implicit processorMode: ProcessorMode): LoadStoreMultiple with UpdateBase =
    ImmedUpdateBase(condition, registers, baseRegister, addressingMode)

  private def ImmedUpdateBase(condition: Condition, registers: Seq[GeneralRegister], baseRegister: GeneralRegister,
                              addressingMode: UpdateMode) =
    new LoadStoreMultiple(LoadStoreMultipleDirection.Store, condition, registers, baseRegister, addressingMode, opcode)
      with UpdateBase

  def withUserModeRegisters(registers: Seq[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode,
                            condition: Condition = Always)(implicit processorMode: ProcessorMode): LoadStoreMultiple with UserModeRegisters =
    ImmedUserModeRegisters(condition, registers, baseRegister, addressingMode)

  private def ImmedUserModeRegisters(condition: Condition, registers: Seq[GeneralRegister], baseRegister: GeneralRegister,
                                     addressingMode: UpdateMode) =
    new LoadStoreMultiple(LoadStoreMultipleDirection.Store, condition, registers, baseRegister, addressingMode, opcode)
      with UserModeRegisters
}

object ReturnFromException {
  implicit val opcode: String = "rfe"

  def apply(baseRegister: GeneralRegister, addressingMode: UpdateMode)(implicit processorMode: ProcessorMode): ReturnFromExceptionOperation =
    Immed(baseRegister, addressingMode, updateBase = false)

  private def Immed(baseRegister: GeneralRegister, addressingMode: UpdateMode, updateBase: Boolean) =
    new ReturnFromExceptionOperation(baseRegister, addressingMode, updateBase, opcode)

  def withUpdateBase(baseRegister: GeneralRegister, addressingMode: UpdateMode)(implicit processorMode: ProcessorMode): ReturnFromExceptionOperation =
    Immed(baseRegister, addressingMode, updateBase = true)
}

object Push {
  def apply(registers: Seq[GeneralRegister])(implicit processorMode: ProcessorMode): LoadStoreMultiple with UpdateBase =
    LoadMultiple.withUpdateBase(registers, GeneralRegister.SP, UpdateMode.DecrementAfter)
}

object Pop {
  def apply(registers: Seq[GeneralRegister])(implicit processorMode: ProcessorMode): LoadStoreMultiple with UpdateBase =
    StoreMultiple.withUpdateBase(registers, GeneralRegister.SP, UpdateMode.IncrementBefore)
}
