package assembler.arm.instructions

import assembler.arm.ProcessorMode
import assembler.arm.opcodes._
import assembler.arm.opcodes.LoadStoreOperation.LoadStoreOperation
import assembler.arm.operands.Condition._
import assembler.arm.operands.registers.GeneralRegister

class LoadStoreRegister(
    wordOperation: LoadStoreOperation, byteOperation: LoadStoreOperation)(implicit val mnemnonic: String) {

  private val ImmedWord = new LoadStore(wordOperation)
  private val ImmedByte = new LoadStore(byteOperation)

  def apply(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreOffset, addressingType: LoadStoreAddressingTypeNormal, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    ImmedWord(condition, register, baseRegister, offset, addressingType)

  def byte(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreOffset, addressingType: LoadStoreAddressingTypeNormal, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    ImmedByte(condition, register, baseRegister, offset, addressingType)

  object UserMode {
    def apply(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreOffset, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
      ImmedWord(condition, register, baseRegister, offset, LoadStoreAddressingTypeUser.PostIndexedUser)

    def byte(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreOffset, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
      ImmedByte(condition, register, baseRegister, offset, LoadStoreAddressingTypeUser.PostIndexedUser)
  }
}

object LoadRegister extends LoadStoreRegister(LoadStoreOperation.LoadWord, LoadStoreOperation.LoadByte)("ldr") {
  private val ImmedDoubleWord = new LoadStoreMiscelaneous(LoadStoreMiscelaneousOperation.LoadDoubleWord)
  private val ImmedUnsignedHalfWord = new LoadStoreMiscelaneous(LoadStoreMiscelaneousOperation.LoadUnsignedHalfWord)
  private val ImmedSignedByte = new LoadStoreMiscelaneous(LoadStoreMiscelaneousOperation.LoadSignedByte)
  private val ImmedSignedHalfWord = new LoadStoreMiscelaneous(LoadStoreMiscelaneousOperation.LoadSignedHalfWord)

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
  private val ImmedHalfWord = new LoadStoreMiscelaneous(LoadStoreMiscelaneousOperation.StoreHalfWord)
  private val ImmedDoubleWord = new LoadStoreMiscelaneous(LoadStoreMiscelaneousOperation.StoreDoubleWord)

  def halfWord(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreMiscelaneousOffset, addressingType: LoadStoreAddressingTypeNormal, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    ImmedHalfWord(condition, register, baseRegister, offset, addressingType)

  def doubleWord(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreMiscelaneousOffset, addressingType: LoadStoreAddressingTypeNormal, condition: Condition = Always)(implicit processorMode: ProcessorMode) =
    ImmedDoubleWord(condition, register, baseRegister, offset, addressingType)
}