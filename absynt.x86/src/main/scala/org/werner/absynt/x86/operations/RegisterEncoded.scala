package org.werner.absynt.x86.operations

import org.werner.absynt.x86.operands.{GeneralPurposeRegister, ValueSize}
import org.werner.absynt.x86.operations.OperandInfo.OperandOrder.OperandOrder

abstract class RegisterEncoded[Size <: ValueSize](register: GeneralPurposeRegister with Size,
                                                              rawCode: Seq[Byte],
                                                              override val mnemonic: String)
                                                             (implicit operandSizePrefixRequirement: OperandSizePrefixRequirement)
  extends X86Operation(rawCode.take(rawCode.length - 1) :+ (rawCode.last | register.registerCode).toByte) with NoModRM {

  self: DisplacementBytes with ImmediateBytes =>
  def registerOrder: OperandOrder

  protected override def allOperands: Set[OperandInfo[_]] =
    super.allOperands + OperandInfo.encodedRegister(register, registerOrder)
}
