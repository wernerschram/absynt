package assembler.arm.instructions

import assembler.arm.ProcessorMode
import assembler.arm.operands.ArmRelativeOffset
import assembler.arm.operands.Condition._
import assembler.arm.operands.registers.GeneralRegister
import assembler.arm.operations.LoadStoreOperation.LoadStoreOperation
import assembler.arm.operations._
import assembler.resource.{UnlabeledEncodable, RelativeReference}
import assembler.{Label, OffsetDirection, RelativeOffsetDirection}

abstract class LoadStoreReference(val opcode: String, target: Label, val condition: Condition)
  extends RelativeReference(target) with NamedConditional {

  override def sizeForDependencySize(distance: Int, offsetDirection: OffsetDirection): Int = 4

  override def possibleSizes: Set[Int] = Set(4)

  override def toString = s"$mnemonicString $target"
}

class LoadStoreRegister(
    wordOperation: LoadStoreOperation, byteOperation: LoadStoreOperation)(implicit val mnemonic: String) {

  private def ImmedWord(condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
                        offset: LoadStoreOffset, addressingType: LoadStoreAddressingType) =
    new LoadStore(mnemonic, condition, register, baseRegister, offset, addressingType, wordOperation)

  private def ImmedByte(condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
                        offset: LoadStoreOffset, addressingType: LoadStoreAddressingType) =
    new LoadStore(mnemonic, condition, register, baseRegister, offset, addressingType, byteOperation)

  def apply(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreOffset = LoadStoreOffset.noOffset,
            addressingType: LoadStoreAddressingTypeNormal = LoadStoreAddressingTypeNormal.OffsetNormal, condition: Condition = Always)
           (implicit processorMode: ProcessorMode): LoadStore =
    ImmedWord(condition, register, baseRegister, offset, addressingType)

  def byte(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreOffset = LoadStoreOffset.noOffset,
           addressingType: LoadStoreAddressingTypeNormal = LoadStoreAddressingTypeNormal.OffsetNormal, condition: Condition = Always)
          (implicit processorMode: ProcessorMode): LoadStore =
    ImmedByte(condition, register, baseRegister, offset, addressingType)

  def apply(targetLabel: Label, destination: GeneralRegister): RelativeReference =
    new LoadStoreReference(mnemonic, targetLabel, Always) {
      override def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): UnlabeledEncodable =
        ImmedWord(Always, destination, GeneralRegister.PC,
          LoadStoreOffset(ArmRelativeOffset.positionalOffset(distance)(offsetDirection).offset.toShort),
            LoadStoreAddressingTypeNormal.OffsetNormal)
    }

  def apply(targetLabel: Label, destination: GeneralRegister, condition: Condition): RelativeReference =
    new LoadStoreReference(mnemonic, targetLabel, condition) {
      override def encodableForDistance(distance: Int, offsetDirection: RelativeOffsetDirection): UnlabeledEncodable =
        ImmedWord(condition, destination, GeneralRegister.PC,
          LoadStoreOffset(ArmRelativeOffset.positionalOffset(distance)(offsetDirection).offset.toShort),
            LoadStoreAddressingTypeNormal.OffsetNormal)
    }

  object UserMode {
    def apply(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreOffset, condition: Condition = Always)
             (implicit processorMode: ProcessorMode): LoadStore =
      ImmedWord(condition, register, baseRegister, offset, LoadStoreAddressingTypeUser.PostIndexedUser)

    def byte(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreOffset, condition: Condition = Always)
            (implicit processorMode: ProcessorMode): LoadStore =
      ImmedByte(condition, register, baseRegister, offset, LoadStoreAddressingTypeUser.PostIndexedUser)
  }
}

object LoadRegister extends LoadStoreRegister(LoadStoreOperation.LoadWord, LoadStoreOperation.LoadByte)("ldr") {
  private def ImmedDoubleWord(condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
                              offset: LoadStoreMiscellaneousOffset, addressingType: LoadStoreAddressingType) =
    new LoadStoreMiscelaneous("ldr", condition, register, baseRegister, offset, addressingType,
      LoadStoreMiscellaneousOperation.LoadDoubleWord)

  private def ImmedUnsignedHalfWord(condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
                                    offset: LoadStoreMiscellaneousOffset, addressingType: LoadStoreAddressingType) =
    new LoadStoreMiscelaneous("ldr", condition, register, baseRegister, offset, addressingType,
      LoadStoreMiscellaneousOperation.LoadUnsignedHalfWord)

  private def ImmedSignedByte(condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
                              offset: LoadStoreMiscellaneousOffset, addressingType: LoadStoreAddressingType) =
    new LoadStoreMiscelaneous("ldr", condition, register, baseRegister, offset, addressingType,
      LoadStoreMiscellaneousOperation.LoadSignedByte)

  private def ImmedSignedHalfWord(condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
                                  offset: LoadStoreMiscellaneousOffset, addressingType: LoadStoreAddressingType) =
    new LoadStoreMiscelaneous("ldr", condition, register, baseRegister, offset, addressingType,
      LoadStoreMiscellaneousOperation.LoadSignedHalfWord)

  def doubleWord(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreMiscellaneousOffset,
                 addressingType: LoadStoreAddressingTypeNormal, condition: Condition = Always)
                (implicit processorMode: ProcessorMode): LoadStoreMiscelaneous =
    ImmedDoubleWord(condition, register, baseRegister, offset, addressingType)

  def signedByte(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreMiscellaneousOffset,
                 addressingType: LoadStoreAddressingTypeNormal, condition: Condition = Always)
                (implicit processorMode: ProcessorMode): LoadStoreMiscelaneous =
    ImmedSignedByte(condition, register, baseRegister, offset, addressingType)

  def unsignedHalfWord(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreMiscellaneousOffset,
                       addressingType: LoadStoreAddressingTypeNormal, condition: Condition = Always)
                      (implicit processorMode: ProcessorMode): LoadStoreMiscelaneous =
    ImmedUnsignedHalfWord(condition, register, baseRegister, offset, addressingType)

  def signedHalfWord(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreMiscellaneousOffset,
                     addressingType: LoadStoreAddressingTypeNormal, condition: Condition = Always)
                    (implicit processorMode: ProcessorMode): LoadStoreMiscelaneous =
    ImmedSignedHalfWord(condition, register, baseRegister, offset, addressingType)
}

object StoreRegister extends LoadStoreRegister(LoadStoreOperation.StoreWord, LoadStoreOperation.StoreByte)("str") {
  private def ImmedHalfWord(condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
                            offset: LoadStoreMiscellaneousOffset, addressingType: LoadStoreAddressingType) =
    new LoadStoreMiscelaneous("str", condition, register, baseRegister, offset, addressingType,
      LoadStoreMiscellaneousOperation.StoreHalfWord)

  private def ImmedDoubleWord(condition: Condition, register: GeneralRegister, baseRegister: GeneralRegister,
                              offset: LoadStoreMiscellaneousOffset, addressingType: LoadStoreAddressingType) =
    new LoadStoreMiscelaneous("str", condition, register, baseRegister, offset, addressingType,
      LoadStoreMiscellaneousOperation.StoreDoubleWord)

  def halfWord(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreMiscellaneousOffset,
               addressingType: LoadStoreAddressingTypeNormal, condition: Condition = Always)
              (implicit processorMode: ProcessorMode): LoadStoreMiscelaneous =
    ImmedHalfWord(condition, register, baseRegister, offset, addressingType)

  def doubleWord(register: GeneralRegister, baseRegister: GeneralRegister, offset: LoadStoreMiscellaneousOffset,
                 addressingType: LoadStoreAddressingTypeNormal, condition: Condition = Always)
                (implicit processorMode: ProcessorMode): LoadStoreMiscelaneous =
    ImmedDoubleWord(condition, register, baseRegister, offset, addressingType)
}