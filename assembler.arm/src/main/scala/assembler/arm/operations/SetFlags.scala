package assembler.arm.operations

import assembler.sections.Section

trait SetFlags extends Conditional {
  self: Conditional =>

  override def encodeWord()(implicit page: Section): Int =
    super.encodeWord() | ARMOperation.sBit

  override def mnemonic: List[PartialName] = PartialName("s", 2) :: super.mnemonic
}
