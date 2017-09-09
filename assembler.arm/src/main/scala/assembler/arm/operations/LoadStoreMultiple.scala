package assembler.arm.operations

import assembler.Label
import assembler.arm.operands.Condition.Condition
import assembler.arm.operands.registers.GeneralRegister
import assembler.sections.Section

class LoadStoreMultipleDirection(val bitMask: Int)

object LoadStoreMultipleDirection {

  object Store extends LoadStoreMultipleDirection(0x00000000)
  object Load extends LoadStoreMultipleDirection(0x00100000)

}

class LoadStoreMultiple(val label: Label, direction: LoadStoreMultipleDirection, val condition: Condition, val registers:
                        List[GeneralRegister], val baseRegister: GeneralRegister, val addressingMode: UpdateMode, val opcode: String)
  extends Conditional {
  assume(registers.nonEmpty)
  assume(baseRegister != GeneralRegister.R15)

  override def encodeWord: Int =
    super.encodeWord | 0x08000000 |
      addressingMode.bitMask | direction.bitMask |
      (baseRegister.registerCode << 16) |
      toRegisterBits(registers)

  def toRegisterBits(registers: List[GeneralRegister]): Int =
    registers.foldLeft(0)((result, instance) => result | (1 << instance.registerCode))

  override def toString = s"$labelPrefix$mnemonicString${addressingMode.mnemonicExtension} $baseRegisterString, $registerString"

  def baseRegisterString: String = baseRegister.toString()

  def registerString = s"{${registers.map { x => x.toString }.mkString(", ")}}"
}

trait UpdateBase extends LoadStoreMultiple {
  self: LoadStoreMultiple =>
  override def encodeWord: Int =
    super.encodeWord | 0x00200000

  override def baseRegisterString = s"${super.baseRegisterString}!"
}

trait UserModeRegisters extends LoadStoreMultiple {
  self: LoadStoreMultiple =>
  assume(!(this.isInstanceOf[UpdateBase] && !registers.contains(GeneralRegister.R15)))

  override def encodeWord: Int =
    super.encodeWord | 0x00400000

  override def registerString = s"${super.registerString}^"
}

class ReturnFromException(val label: Label, baseRegister: GeneralRegister, addressingMode: UpdateMode, updateBase: Boolean,
                          val opcode: String)
  extends ARMOperation() {
  override def encodeWord: Int =
    0xf8100a00 |
      (if (updateBase) 0x00200000 else 0) |
      addressingMode.bitMask |
      (baseRegister.registerCode << 16)

  override def toString = s"$labelPrefix$mnemonicString${addressingMode.mnemonicExtension} $baseRegister${if (updateBase) "!" else ""}"
}
