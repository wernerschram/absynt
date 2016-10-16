package assembler.arm.opcodes

import assembler.arm.ProcessorMode
import assembler.arm.instructions.ARMInstruction
import assembler.arm.instructions.ConditionalARMInstruction
import assembler.arm.operands.Condition.Condition
import assembler.arm.operands.registers.GeneralRegister
import assembler.memory.MemoryPage

class LoadStoreMultipleOperation(val bitmask: Int)

object LoadStoreMultipleOperation {
  object Store extends LoadStoreMultipleOperation(0x00000000)
  object Load extends LoadStoreMultipleOperation(0x00100000)
}

class LoadStoreMultiple(direction: LoadStoreMultipleOperation)(implicit mnemonic: String)
    extends Opcode(mnemonic) {

  def apply(condition: Condition, registers: List[GeneralRegister], baseRegister: GeneralRegister, addressingMode: UpdateMode, updateBase: Boolean, userModeRegisters: Boolean)(implicit processorMode: ProcessorMode): ARMInstruction = {
    assume(!registers.isEmpty)
    assume(baseRegister != GeneralRegister.R15)
    assume(!(updateBase && userModeRegisters && !registers.contains(GeneralRegister.R15)))

    new ConditionalARMInstruction(condition) {
      def toRegisterBits(registers: List[GeneralRegister]): Int =
        registers.aggregate(0)((result, instance) => result | (1 << instance.registerCode), (result1, result2) => result1 | result2)

      override def encodeWord()(implicit page: MemoryPage) =
        (super.encodeWord() | 0x08000000 |
          (if (updateBase) 0x00200000 else 0) |
          (if (userModeRegisters) 0x00400000 else 0) |
          addressingMode.bitMask | direction.bitmask |
          (baseRegister.registerCode << 16) |
          toRegisterBits(registers))

      override lazy val toString = s"${mnemonic}${addressingMode.mnemonicExtension} ${baseRegister}${if (updateBase) "!" else ""}, {${registers.map { x => x.toString }.mkString(", ")}}${if (userModeRegisters) "^" else ""}" // ${value.toString()}}"
    }
  }
}

class ReturnFromException()(implicit mnemonic: String)
    extends Opcode(mnemonic) {

  def apply(baseRegister: GeneralRegister, addressingMode: UpdateMode, updateBase: Boolean)(implicit processorMode: ProcessorMode): ARMInstruction = {
    assume(baseRegister != GeneralRegister.R15)

    new ARMInstruction() {

      override def encodeWord()(implicit page: MemoryPage) =
        (0xf8100a00 |
          (if (updateBase) 0x00200000 else 0) |
          addressingMode.bitMask |
          (baseRegister.registerCode << 16))

      override lazy val toString = s"${mnemonic}${addressingMode.mnemonicExtension} ${baseRegister}${if (updateBase) "!" else ""}"
    }
  }
}