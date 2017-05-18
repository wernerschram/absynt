package assembler.arm.instructions

import assembler.{Encodable, Label}
import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition._
import assembler.arm.operands.registers.GeneralRegister
import assembler.arm.operations.LoadStoreOperation.LoadStoreOperation
import assembler.arm.operations._
import assembler.sections.Section

class LoadStoreRegister(
    wordOperation: LoadStoreOperation, byteOperation: LoadStoreOperation)(implicit val mnemnonic: String) {

  private def ImmedWord(label: Label, condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
                        offset: LoadStoreOffset, addressingType: LoadStoreAddressingType) =
    new LoadStore(label, mnemnonic, condition, register, baseRegister, offset, addressingType, wordOperation)

  private def ImmedByte(label: Label, condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
                        offset: LoadStoreOffset, addressingType: LoadStoreAddressingType) =
    new LoadStore(label, mnemnonic, condition, register, baseRegister, offset, addressingType, byteOperation)

  def apply(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreOffset = LoadStoreOffset.noOffset,
            addressingType: LoadStoreAddressingTypeNormal = LoadStoreAddressingTypeNormal.OffsetNormal, condition: Condition = Always)
           (implicit label: Label, processorMode: ProcessorMode) =
    ImmedWord(label, condition, register, baseRegister, offset, addressingType)

  def byte(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreOffset = LoadStoreOffset.noOffset,
           addressingType: LoadStoreAddressingTypeNormal = LoadStoreAddressingTypeNormal.OffsetNormal, condition: Condition = Always)
          (implicit label: Label, processorMode: ProcessorMode) =
    ImmedByte(label, condition, register, baseRegister, offset, addressingType)

  def apply(targetLabel: Label, destination: GeneralRegister)(implicit label: Label, processorMode: ProcessorMode) =
    new ReferencingARMOperation(label, mnemnonic, targetLabel, Always) {
      override def encodableForDistance(distance: Int)(implicit page: Section): Encodable =
        ImmedWord(label, Always, destination, GeneralRegister.PC, LoadStoreOffset(distance.toByte), LoadStoreAddressingTypeNormal.OffsetNormal)
    }

  def apply(targetLabel: Label, destination: GeneralRegister, condition: Condition)(implicit label: Label, processorMode: ProcessorMode) =
    new ReferencingARMOperation(label, mnemnonic, targetLabel, condition) {
      override def encodableForDistance(distance: Int)(implicit page: Section): Encodable =
        ImmedWord(label, condition, destination, GeneralRegister.PC, LoadStoreOffset(distance.toByte),
          LoadStoreAddressingTypeNormal.OffsetNormal)
    }

  object UserMode {
    def apply(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreOffset, condition: Condition = Always)
             (implicit label: Label, processorMode: ProcessorMode) =
      ImmedWord(label, condition, register, baseRegister, offset, LoadStoreAddressingTypeUser.PostIndexedUser)

    def byte(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreOffset, condition: Condition = Always)
            (implicit label: Label, processorMode: ProcessorMode) =
      ImmedByte(label, condition, register, baseRegister, offset, LoadStoreAddressingTypeUser.PostIndexedUser)
  }
}

object LoadRegister extends LoadStoreRegister(LoadStoreOperation.LoadWord, LoadStoreOperation.LoadByte)("ldr") {
  private def ImmedDoubleWord(label: Label, condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
                              offset: LoadStoreMiscelaneousOffset, addressingType: LoadStoreAddressingType) =
    new LoadStoreMiscelaneous(label, "ldr", condition, register, baseRegister, offset, addressingType,
      LoadStoreMiscellaneousOperation.LoadDoubleWord)

  private def ImmedUnsignedHalfWord(label: Label, condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
                                    offset: LoadStoreMiscelaneousOffset, addressingType: LoadStoreAddressingType) =
    new LoadStoreMiscelaneous(label, "ldr", condition, register, baseRegister, offset, addressingType,
      LoadStoreMiscellaneousOperation.LoadUnsignedHalfWord)

  private def ImmedSignedByte(label: Label, condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
                              offset: LoadStoreMiscelaneousOffset, addressingType: LoadStoreAddressingType) =
    new LoadStoreMiscelaneous(label, "ldr", condition, register, baseRegister, offset, addressingType,
      LoadStoreMiscellaneousOperation.LoadSignedByte)

  private def ImmedSignedHalfWord(label: Label, condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
                                  offset: LoadStoreMiscelaneousOffset, addressingType: LoadStoreAddressingType) =
    new LoadStoreMiscelaneous(label, "ldr", condition, register, baseRegister, offset, addressingType,
      LoadStoreMiscellaneousOperation.LoadSignedHalfWord)

  def doubleWord(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreMiscelaneousOffset,
                 addressingType: LoadStoreAddressingTypeNormal, condition: Condition = Always)
                (implicit label: Label, processorMode: ProcessorMode) =
    ImmedDoubleWord(label, condition, register, baseRegister, offset, addressingType)

  def signedByte(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreMiscelaneousOffset,
                 addressingType: LoadStoreAddressingTypeNormal, condition: Condition = Always)
                (implicit label: Label, processorMode: ProcessorMode) =
    ImmedSignedByte(label, condition, register, baseRegister, offset, addressingType)

  def unsignedHalfWord(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreMiscelaneousOffset,
                       addressingType: LoadStoreAddressingTypeNormal, condition: Condition = Always)
                      (implicit label: Label, processorMode: ProcessorMode) =
    ImmedUnsignedHalfWord(label, condition, register, baseRegister, offset, addressingType)

  def signedHalfWord(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreMiscelaneousOffset,
                     addressingType: LoadStoreAddressingTypeNormal, condition: Condition = Always)
                    (implicit label: Label, processorMode: ProcessorMode) =
    ImmedSignedHalfWord(label, condition, register, baseRegister, offset, addressingType)
}

object StoreRegister extends LoadStoreRegister(LoadStoreOperation.StoreWord, LoadStoreOperation.StoreByte)("str") {
  private def ImmedHalfWord(label: Label, condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
                            offset: LoadStoreMiscelaneousOffset, addressingType: LoadStoreAddressingType) =
    new LoadStoreMiscelaneous(label, "str", condition, register, baseRegister, offset, addressingType,
      LoadStoreMiscellaneousOperation.StoreHalfWord)

  private def ImmedDoubleWord(label: Label, condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
                              offset: LoadStoreMiscelaneousOffset, addressingType: LoadStoreAddressingType) =
    new LoadStoreMiscelaneous(label, "str", condition, register, baseRegister, offset, addressingType,
      LoadStoreMiscellaneousOperation.StoreDoubleWord)

  def halfWord(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreMiscelaneousOffset,
               addressingType: LoadStoreAddressingTypeNormal, condition: Condition = Always)
              (implicit label: Label, processorMode: ProcessorMode) =
    ImmedHalfWord(label, condition, register, baseRegister, offset, addressingType)

  def doubleWord(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreMiscelaneousOffset,
                 addressingType: LoadStoreAddressingTypeNormal, condition: Condition = Always)
                (implicit label: Label, processorMode: ProcessorMode) =
    ImmedDoubleWord(label, condition, register, baseRegister, offset, addressingType)
}