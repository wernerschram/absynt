package assembler.x86.operands

trait ModRMEncodableOperand extends Operand {
  val modValue: Byte
  val registerOrMemoryModeCode: Byte

  def getExtendedBytes(rValue: Byte): Seq[Byte] = Seq(getModRM(rValue))

  private def getModRM(rValue: Byte): Byte = (((modValue & 3) << 6) | ((rValue & 7) << 3) | (registerOrMemoryModeCode & 7)).toByte
}

trait FixedSizeOperand {
  def operandByteSize: OperandSize
}
