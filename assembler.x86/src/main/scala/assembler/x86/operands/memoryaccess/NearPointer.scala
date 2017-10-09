package assembler.x86.operands.memoryaccess

import assembler.Address
import assembler.ListExtensions._
import assembler.x86.ProcessorModeWithOffset
import assembler.x86.operands.{FixedSizeOperand, Operand, OperandSize, ValueSize}

sealed abstract class NearPointer[OffsetType <: X86Offset : ProcessorModeWithOffset](val offset: OffsetType)
  extends Address[OffsetType] with Operand with FixedSizeOperand {
  val operandByteSize: OperandSize


  def encodeBytes: List[Byte]

  override def add(that: OffsetType): NearPointer[OffsetType]
}

object ShortPointer {
  def apply[OffsetType <: X86Offset : ProcessorModeWithOffset](offset: OffsetType): NearPointer[OffsetType] = new NearPointer[OffsetType](offset) {
    val operandByteSize: ValueSize = ValueSize.Byte
    override def encodeBytes: List[Byte] = offset.encodeShort
    override def toString: String = s"0x${offset.encodeShort.bigEndianHexString}"
    override def add(that: OffsetType): NearPointer[OffsetType] = apply(that + offset)
  }
}

object LongPointer {
  def apply[OffsetType <: X86Offset : ProcessorModeWithOffset](offset: OffsetType): NearPointer[OffsetType] = new NearPointer(offset) {
    val operandByteSize: OperandSize = offset.operandByteSize
    override def encodeBytes: List[Byte] = offset.encode
    override def toString: String = s"0x${offset.encode.bigEndianHexString}"
    override def add(that: OffsetType): NearPointer[OffsetType] = apply(that + offset)
  }
}