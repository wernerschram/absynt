package org.werner.absynt.x86.operations

import org.werner.absynt.x86.operations.OperandInfo.OperandOrder.destination


sealed trait Operands {
  self: X86Operation =>

  protected override def allOperands: Set[OperandInfo[?]]
}

trait ExtraOperands(operands: OperandInfo[?]*) extends Operands {
  self: X86Operation =>

  protected override abstract def allOperands: Set[OperandInfo[?]] =
    super.allOperands ++ operands
}
