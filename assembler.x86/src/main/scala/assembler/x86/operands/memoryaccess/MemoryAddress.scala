package assembler.x86.operands.memoryaccess

import assembler.ListExtensions.ListToImmediate
import assembler.x86.operands._
import assembler.x86.operations.AddressOperandInfo

sealed class MemoryAddress private(address: ImmediateValue, segment: SegmentRegister = Register.DS)
  extends MemoryLocation(Some(address), segment, ValueSize.sizeOfValue(address.value.size)) with ModRMEncodableOperand {

  override val addressOperands: Seq[AddressOperandInfo] = Seq(AddressOperandInfo.rmDisplacement(address))

  override val modValue: Byte = 0x00.toByte

  override val registerOrMemoryModeCode: Byte = if (addressSize == ValueSize.Word) 0x06.toByte else 0x05.toByte
  override val defaultSegment: SegmentRegister = Register.DS

  override def getExtendedBytes(rValue: Byte): Seq[Byte] = super.getExtendedBytes(rValue) ++ address.value

  override def toString = s"$segmentPrefix[${address.value.decimalString}]"
}

object MemoryAddress {

  def apply(address: ImmediateValue, segment: SegmentRegister = Register.DS) =
    new MemoryAddress(address, segment)

  def withSize(address: ImmediateValue, segment: SegmentRegister = Register.DS)(size: ValueSize): MemoryAddress with FixedSizeOperand =
    new MemoryAddress(address, segment) with FixedSizeOperand {
      override val operandByteSize: OperandSize = size

      override def toString = s"$operandByteSize PTR ${super.toString}"
    }
}