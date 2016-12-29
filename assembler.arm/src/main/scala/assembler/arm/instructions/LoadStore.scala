package assembler.arm.instructions

import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition._
import assembler.arm.operands.registers.GeneralRegister
import assembler.arm.operations._
import assembler.arm.operations.LoadStoreOperation.LoadStoreOperation

class LoadStoreRegister(
    wordOperation: LoadStoreOperation, byteOperation: LoadStoreOperation)(implicit val mnemnonic: String) {

  private def ImmedWord(condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreOffset,
    addressingType: LoadStoreAddressingType) =
    new LoadStore(mnemnonic, condition, register, baseRegister, offset, addressingType, wordOperation)

  private def ImmedByte(condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreOffset,
    addressingType: LoadStoreAddressingType) =
    new LoadStore(mnemnonic, condition, register, baseRegister, offset, addressingType, byteOperation)

  def apply(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreOffset = LoadStoreOffset.noOffset, addressingType: LoadStoreAddressingTypeNormal = LoadStoreAddressingTypeNormal.OffsetNormal, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    ImmedWord(condition, register, baseRegister, offset, addressingType)

  def byte(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreOffset = LoadStoreOffset.noOffset, addressingType: LoadStoreAddressingTypeNormal = LoadStoreAddressingTypeNormal.OffsetNormal, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    ImmedByte(condition, register, baseRegister, offset, addressingType)

  object UserMode {
    def apply(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreOffset, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
      ImmedWord(condition, register, baseRegister, offset, LoadStoreAddressingTypeUser.PostIndexedUser)

    def byte(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreOffset, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
      ImmedByte(condition, register, baseRegister, offset, LoadStoreAddressingTypeUser.PostIndexedUser)
  }
}

object LoadRegister extends LoadStoreRegister(LoadStoreOperation.LoadWord, LoadStoreOperation.LoadByte)("ldr") {
  private def ImmedDoubleWord(condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
      offset: LoadStoreMiscelaneousOffset, addressingType: LoadStoreAddressingType) =
    new LoadStoreMiscelaneous("ldr", condition, register, baseRegister, offset, addressingType, LoadStoreMiscelaneousOperation.LoadDoubleWord)
  private def ImmedUnsignedHalfWord(condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
      offset: LoadStoreMiscelaneousOffset, addressingType: LoadStoreAddressingType) =
    new LoadStoreMiscelaneous("ldr", condition, register, baseRegister, offset, addressingType, LoadStoreMiscelaneousOperation.LoadUnsignedHalfWord)
  private def ImmedSignedByte(condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
      offset: LoadStoreMiscelaneousOffset, addressingType: LoadStoreAddressingType) =
    new LoadStoreMiscelaneous("ldr", condition, register, baseRegister, offset, addressingType, LoadStoreMiscelaneousOperation.LoadSignedByte)
  private def ImmedSignedHalfWord(condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
      offset: LoadStoreMiscelaneousOffset, addressingType: LoadStoreAddressingType) =
    new LoadStoreMiscelaneous("ldr", condition, register, baseRegister, offset, addressingType, LoadStoreMiscelaneousOperation.LoadSignedHalfWord)

  def doubleWord(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreMiscelaneousOffset, addressingType: LoadStoreAddressingTypeNormal, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    ImmedDoubleWord(condition, register, baseRegister, offset, addressingType)

  def signedByte(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreMiscelaneousOffset, addressingType: LoadStoreAddressingTypeNormal, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    ImmedSignedByte(condition, register, baseRegister, offset, addressingType)

  def unsignedHalfWord(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreMiscelaneousOffset, addressingType: LoadStoreAddressingTypeNormal, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    ImmedUnsignedHalfWord(condition, register, baseRegister, offset, addressingType)

  def signedHalfWord(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreMiscelaneousOffset, addressingType: LoadStoreAddressingTypeNormal, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    ImmedSignedHalfWord(condition, register, baseRegister, offset, addressingType)
}

object StoreRegister extends LoadStoreRegister(LoadStoreOperation.StoreWord, LoadStoreOperation.StoreByte)("str") {
  private def ImmedHalfWord(condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
      offset: LoadStoreMiscelaneousOffset, addressingType: LoadStoreAddressingType) =
    new LoadStoreMiscelaneous("str", condition, register, baseRegister, offset, addressingType, LoadStoreMiscelaneousOperation.StoreHalfWord)
  private def ImmedDoubleWord(condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
      offset: LoadStoreMiscelaneousOffset, addressingType: LoadStoreAddressingType) =
    new LoadStoreMiscelaneous("str", condition, register, baseRegister, offset, addressingType, LoadStoreMiscelaneousOperation.StoreDoubleWord)

  def halfWord(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreMiscelaneousOffset, addressingType: LoadStoreAddressingTypeNormal, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    ImmedHalfWord(condition, register, baseRegister, offset, addressingType)

  def doubleWord(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreMiscelaneousOffset, addressingType: LoadStoreAddressingTypeNormal, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    ImmedDoubleWord(condition, register, baseRegister, offset, addressingType)
}