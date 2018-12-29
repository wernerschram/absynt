package assembler.x86.operands.memoryaccess

import assembler.ListExtensions.ListToImmediate
import assembler.x86.operands._
import assembler.x86.operations.AddressOperandInfo

sealed class MemoryAddress private(address: ImmediateValue, segment: SegmentRegister = Register.DS)
  extends MemoryLocation(Some(address), segment) with ModRMEncodableOperand {

  override val modValue: Byte = 0x00.toByte

  override val registerOrMemoryModeCode: Byte = if (address.isInstanceOf[WordSize]) 0x06.toByte else 0x05.toByte
  final override val defaultSegment: SegmentRegister = Register.DS

  override val addressOperands: Set[AddressOperandInfo] = Set(AddressOperandInfo.rmDisplacement(address, segmentOverride))

  override def getExtendedBytes(rValue: Byte): Seq[Byte] = super.getExtendedBytes(rValue) ++ address.value

  override def toString = s"$segmentPrefix[${address.value.decimalString}]"
}

object MemoryAddress {

  def apply(address: ImmediateValue, segment: SegmentRegister = Register.DS) =
    new MemoryAddress(address, segment)

  def byteSize(address: ImmediateValue, segment: SegmentRegister = Register.DS) =
    new MemoryAddress(address, segment) with ByteSize

  def wordSize(address: ImmediateValue, segment: SegmentRegister = Register.DS) =
    new MemoryAddress(address, segment) with WordSize

  def doubleWordSize(address: ImmediateValue, segment: SegmentRegister = Register.DS) =
    new MemoryAddress(address, segment) with DoubleWordSize

  def quadWordSize(address: ImmediateValue, segment: SegmentRegister = Register.DS) =
    new MemoryAddress(address, segment) with QuadWordSize

}