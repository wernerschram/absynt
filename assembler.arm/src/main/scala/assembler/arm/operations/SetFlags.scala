package assembler.arm.operations

import assembler.memory.MemoryPage

trait SetFlags extends ConditionalARMOperation {
  self: ConditionalARMOperation =>

  override def encodeWord()(implicit page: MemoryPage) =
    (super.encodeWord() | ARMOperation.sBit)

  abstract override def mnemonic = s"${super.mnemonic}s"
}
