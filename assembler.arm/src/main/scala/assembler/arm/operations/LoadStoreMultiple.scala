package assembler.arm.operations

import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition.Condition
import assembler.arm.operands.registers.GeneralRegister
import assembler.memory.MemoryPage

class LoadStoreMultipleOperation(val bitmask: Int)

object LoadStoreMultipleOperation {
  object Store extends LoadStoreMultipleOperation(0x00000000)
  object Load extends LoadStoreMultipleOperation(0x00100000)
}

//  def apply()(implicit processorMode: ProcessorMode): ARMOperation = {

class LoadStoreMultiple(direction: LoadStoreMultipleOperation, condition: Condition, registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode, updateBase: Boolean, userModeRegisters: Boolean, opcode: String)
    extends ConditionalARMOperation(condition) {
  assume(!registers.isEmpty)
  assume(baseRegister != GeneralRegister.R15)
  assume(!(updateBase && userModeRegisters && !registers.contains(GeneralRegister.R15)))

  override def mnemonic = opcode

  def toRegisterBits(registers: List[GeneralRegister]): Int =
    registers.foldLeft(0)((result, instance) => result | (1 << instance.registerCode))

  override def encodeWord()(implicit page: MemoryPage) =
    (super.encodeWord() | 0x08000000 |
      (if (updateBase) 0x00200000 else 0) |
      (if (userModeRegisters) 0x00400000 else 0) |
      addressingMode.bitMask | direction.bitmask |
      (baseRegister.registerCode << 16) |
      toRegisterBits(registers))

  override def toString = s"${mnemonic}${addressingMode.mnemonicExtension} ${baseRegister}${if (updateBase) "!" else ""}, {${registers.map { x => x.toString }.mkString(", ")}}${if (userModeRegisters) "^" else ""}" // ${value.toString()}}"
}

class ReturnFromException(baseRegister: GeneralRegister, addressingMode: UpdateMode, updateBase: Boolean, opcode: String)
    extends ARMOperation() {

  override def encodeWord()(implicit page: MemoryPage) =
    (0xf8100a00 |
      (if (updateBase) 0x00200000 else 0) |
      addressingMode.bitMask |
      (baseRegister.registerCode << 16))

  override def toString = s"${opcode}${addressingMode.mnemonicExtension} ${baseRegister}${if (updateBase) "!" else ""}"
}
