package org.werner.absynt.x86

sealed protected class RexRequirement private (val rexBitMask: Byte)

object RexRequirement {
  val instanceIndex = new RexRequirement(0x02)
  val instanceBase = new RexRequirement(0x01)
  val instanceOpcodeReg = new RexRequirement(0x01)
  val instanceOperandR = new RexRequirement(0x04)
  val instanceOperandRM = new RexRequirement(0x01)
  val quadOperand = new RexRequirement(0x08)
}
