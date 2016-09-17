package assembler.x86.operands

import assembler.x86.operands.registers._

trait EncodableOperand extends Operand {
  def getExtendedBytes(rValue: Byte): List[Byte] 
  
  def getExtendedBytes(register: EncodableRegister): List[Byte] = 
    getExtendedBytes(register.registerOrMemoryModeCode)

  def getExtendedBytes(segment: SegmentRegister): List[Byte] = 
    getExtendedBytes(segment.registerCode)
  
}

trait ModRMEncodableOperand extends EncodableOperand {
  val modValue: Byte
  val registerOrMemoryModeCode: Byte
//  val displacement: List[Byte]
  
  def getModRM(rValue: Byte): Byte = (((modValue & 3) << 6) | ((rValue & 7) << 3) | (registerOrMemoryModeCode & 7)).toByte

  def getModRM(register: EncodableRegister): Byte = getModRM(register.registerOrMemoryModeCode)

  def getModRM(register: SegmentRegister): Byte = getModRM(register.registerCode)

  override def getExtendedBytes(rValue: Byte): List[Byte] = getModRM(rValue)  :: Nil//:: displacement
}

trait FixedSizeParameter {
  val operandByteSize: Int
}

trait FixedSizeEncodableOperand extends EncodableOperand with FixedSizeParameter

trait FixedSizeModRMEncodableOperand extends ModRMEncodableOperand with FixedSizeParameter