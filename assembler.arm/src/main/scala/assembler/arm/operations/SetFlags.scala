package assembler.arm.operations

import assembler.memory.MemoryPage

trait SetFlags extends Conditional {
  self: Conditional =>

  override def encodeWord()(implicit page: MemoryPage) =
    (super.encodeWord() | ARMOperation.sBit)

  override def mnemonic = PartialName("s", 2) :: super.mnemonic
}
