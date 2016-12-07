package assembler.arm.operations

import assembler.arm.ProcessorMode
import assembler.arm.operands.Condition.Condition
import assembler.arm.operands.registers.GeneralRegister
import assembler.memory.MemoryPage

class LoadStoreMultipleDirection(val bitmask: Int)

object LoadStoreMultipleDirection {
  object Store extends LoadStoreMultipleDirection(0x00000000)
  object Load extends LoadStoreMultipleDirection(0x00100000)
}
//
//abstract class LoadStoreMultipleOperation(baseRegister: GeneralRegister, addressingMode: UpdateMode, val opcode: String)
//  extends ARMOperation {
//
//  override def encodeWord()(implicit page: MemoryPage) =
//    (0xf8100a00 |
//      addressingMode.bitMask |
//      (baseRegister.registerCode << 16))
//
//  def baseRegisterString = baseRegister.toString()
//
//  override def toString = s"${opcode}${addressingMode.mnemonicExtension} ${baseRegisterString}"
//
//}

class LoadStoreMultiple(direction: LoadStoreMultipleDirection, condition: Condition, val registers: List[GeneralRegister], val baseRegister: GeneralRegister, val addressingMode: UpdateMode, opcode: String)
    extends ConditionalARMOperation(condition) {
  assume(!registers.isEmpty)
  assume(baseRegister != GeneralRegister.R15)

  override def mnemonic = opcode

  def toRegisterBits(registers: List[GeneralRegister]): Int =
    registers.foldLeft(0)((result, instance) => result | (1 << instance.registerCode))

  override def encodeWord()(implicit page: MemoryPage) =
    (super.encodeWord() | 0x08000000 |
      addressingMode.bitMask | direction.bitmask |
      (baseRegister.registerCode << 16) |
      toRegisterBits(registers))

  def baseRegisterString = baseRegister.toString()
  def registerString = s"{${registers.map { x => x.toString }.mkString(", ")}}"

  override def toString = s"${mnemonic}${addressingMode.mnemonicExtension} ${baseRegisterString}, ${registerString}"
}

trait UpdateBase extends LoadStoreMultiple {
  self: LoadStoreMultiple =>
  override def encodeWord()(implicit page: MemoryPage) =
    super.encodeWord() | 0x00200000

  override def baseRegisterString = s"${super.baseRegisterString}!"
}

trait UserModeRegisters extends LoadStoreMultiple {
  self: LoadStoreMultiple =>
    assume(!(this.isInstanceOf[UpdateBase] && !registers.contains(GeneralRegister.R15)))
  override def encodeWord()(implicit page: MemoryPage) =
    super.encodeWord() | 0x00400000

  override def registerString = s"${super.registerString}^"
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
