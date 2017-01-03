package assembler.arm.operations

import assembler.memory.MemoryPage

trait SetFlags extends Conditional {
  self: Conditional =>

  override def encodeWord()(implicit page: MemoryPage): Int =
    super.encodeWord() | ARMOperation.sBit

  override def mnemonic: List[PartialName] = PartialName("s", 2) :: super.mnemonic
}
