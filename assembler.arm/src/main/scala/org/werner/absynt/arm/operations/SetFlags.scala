package org.werner.absynt.arm.operations

trait SetFlags extends Conditional {
  self: Conditional =>

  override def encodeWord: Int =
    super.encodeWord | ARMOperation.sBit

  override def mnemonic: List[PartialName] = PartialName("s", 2) :: super.mnemonic
}
