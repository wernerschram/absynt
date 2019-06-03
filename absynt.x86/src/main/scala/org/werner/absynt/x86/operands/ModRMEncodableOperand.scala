package org.werner.absynt.x86.operands

import org.werner.absynt.x86.RexRequirement

trait ModRMEncodableOperand extends Operand {
  val modValue: Byte
  val registerOrMemoryModeCode: Byte

  def getExtendedBytes(rValue: Byte): Seq[Byte] = Seq(getModRM(rValue))

  private def getModRM(rValue: Byte): Byte = (((modValue & 3) << 6) | ((rValue & 7) << 3) | (registerOrMemoryModeCode & 7)).toByte

  def rexRequirements(rexRequirement: RexRequirement): Set[RexRequirement] = Set.empty
}
