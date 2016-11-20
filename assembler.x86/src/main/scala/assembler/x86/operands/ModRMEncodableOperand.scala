package assembler.x86.operands

trait EncodableOperand extends Operand {
  def getExtendedBytes(rValue: Byte): List[Byte]
}

trait ModRMEncodableOperand extends EncodableOperand {
  val modValue: Byte
  val registerOrMemoryModeCode: Byte

  private def getModRM(rValue: Byte): Byte = (((modValue & 3) << 6) | ((rValue & 7) << 3) | (registerOrMemoryModeCode & 7)).toByte

  override def getExtendedBytes(rValue: Byte): List[Byte] = getModRM(rValue)  :: Nil
}

trait FixedSizeParameter {
  val operandByteSize: Int
}

trait FixedSizeEncodableOperand extends EncodableOperand with FixedSizeParameter

trait FixedSizeModRMEncodableOperand extends ModRMEncodableOperand with FixedSizeParameter